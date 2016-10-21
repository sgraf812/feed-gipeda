{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| A task scheduler implementation based on
    <http://haskell-distributed.github.io/tutorials/4ch.html#building-a-task-queue>.
    It is however flexible in the number of active slaves, which are discovered
    via the SimpleLocalnet backend of cloud haskell.
    Also, failures are automatically retried.
-}

module FeedGipeda.TaskScheduler
  ( Task
  , start
  , work
  ) where


import           Control.Concurrent                                 (threadDelay)
import           Control.Concurrent.MSemN                           (MSemN)
import qualified Control.Concurrent.MSemN                           as MSemN
import           Control.Distributed.Process                        hiding
                                                                     (call)
import           Control.Distributed.Process.Async
import           Control.Distributed.Process.Node (runProcess, forkProcess)
import qualified Control.Distributed.Backend.P2P as P2P
import           Control.Distributed.Process.Extras                 hiding
                                                                     (call,
                                                                     send)
import           Control.Distributed.Process.Extras.Time
import           Control.Distributed.Process.ManagedProcess         hiding(runProcess, forkProcess)
import           Control.Distributed.Process.Serializable
import           Control.Monad                                      (forever)
import           Data.Binary                                        (Binary)
import qualified Data.ByteString as BS
import           Data.Functor
import           Data.Map                                           (Map)
import qualified Data.Map                                           as Map
import           Data.Maybe                                         (isNothing)
import           Data.Sequence                                      (Seq,
                                                                     ViewL (..),
                                                                     (<|), (|>))
import qualified Data.Sequence                                      as Seq
import           Data.Set                                           (Set)
import qualified Data.Set                                           as Set
import Data.String (fromString)
import           Data.Typeable                                      (Typeable)
import           GHC.Generics                                       (Generic)


{-| A @Task@ contains all information to execute a process on a remote
    node and finalize the result with a continuation. It consists of:

      * A static pointer for the slave to find the entry point of task to execute

      * The state in which to execute the task

      * A continuation to call with the result of the (possibly timed out) task
-}
type Task a
  = (Static (SerializableDict a), Closure (Process a), Maybe a -> IO ())


type RemoteCall a
  = (Static (SerializableDict a), Closure (Process a))


newtype Result a
  = Result a
  deriving (Eq, Ord, Show, Generic, Typeable)
instance Binary a => Binary (Result a)


newtype SlaveListChanged
  = SlaveListChanged (Set NodeId)
  deriving (Ord, Eq, Show, Generic, Typeable)
instance Binary SlaveListChanged


data SchedulerState a
  = SchedulerState
  { slaves :: Map NodeId (Maybe MonitorRef)
  , active :: Map MonitorRef (NodeId, Async a, CallRef (Maybe a), RemoteCall a)
  , onHold :: Seq (CallRef (Maybe a), RemoteCall a)
  }


initialSchedulerState :: SchedulerState a
initialSchedulerState =
  SchedulerState Map.empty Map.empty Seq.empty


{-| Spawn the task queue on the local node and start to discover slave nodes.
    Tasks which don't finish within `timeout` return `Nothing`.

    The consumer tries to keep the task list short and `await`s new task items
    only when there are idle slaves. This way, upstream can react more timely
    to changes in task items priority (e.g. when a newer commit comes in or
    to interleave commits of a new repository).
-}
start
  :: forall a r . Serializable a
  => IO (Task a)
  -> Process ()
start awaitTask = do
  unregister "logger"
  idleSlaves <- liftIO (MSemN.new 0)
  q <- spawnLocal (queue idleSlaves)
  spawnLocal (slaveDiscovery q)
  spawnLocal (dispatch idleSlaves q)
  return ()
    where
      dispatch
        :: MSemN Int
        -> ProcessId
        -> Process ()
      dispatch idleSlaves queue = forever $ do
        liftIO (MSemN.wait idleSlaves 1)
        (dict, closure, cont) <- liftIO awaitTask
        spawnLocal $ do
          ret <- call queue (dict, closure)
          liftIO (cont ret)

      queue :: MSemN Int -> Process ()
      queue idleSlaves =
        serve () init (process idleSlaves)

      init :: InitHandler () (SchedulerState a)
      init () =
        return (InitOk initialSchedulerState Infinity)

      process :: MSemN Int -> ProcessDefinition (SchedulerState a)
      process idleSlaves = defaultProcess
        { apiHandlers =
            [ handleCast (onSlaveListChanged idleSlaves)
            , handleCallFrom onNewTask
            ]
        , infoHandlers =
            [ handleInfo (onTaskCompleted idleSlaves)
            ]
        , unhandledMessagePolicy = Log
        }

      assignTasks :: SchedulerState a -> Process (SchedulerState a)
      assignTasks qs@(SchedulerState slaves active onHold) =
        let
          idle :: Set NodeId
          idle =
            Map.keysSet (Map.filter isNothing slaves)

          assignment :: Maybe (NodeId, CallRef (Maybe a), RemoteCall a)
          assignment = do
            node <- fst <$> Set.minView idle
            (ref, task) <- case Seq.viewl onHold of
              EmptyL -> Nothing
              head :< _ -> Just head
            return (node, ref, task)
        in
          case assignment of
            Nothing -> return qs
            Just (node, callRef, (dict, closure)) -> do
              handle <- async (remoteTask dict node closure)
              monitorRef <- monitorAsync handle
              return qs
                { slaves = Map.insert node (Just monitorRef) slaves
                , onHold = Seq.drop 1 onHold
                , active = Map.insert monitorRef (node, handle, callRef, (dict, closure)) active
                }

      onNewTask
        :: SchedulerState a
        -> CallRef (Maybe a)
        -> RemoteCall a
        -> Process (ProcessReply (Maybe a) (SchedulerState a))
      onNewTask qs ref call =
        assignTasks (qs { onHold = onHold qs |> (ref, call) }) >>= noReply_

      onTaskCompleted
        :: MSemN Int
        -> SchedulerState a
        -> ProcessMonitorNotification
        -> Process (ProcessAction (SchedulerState a))
      onTaskCompleted idleSlaves qs (ProcessMonitorNotification monitorRef _ _) =
        let
          withoutRef =
            Map.delete monitorRef (active qs)
          reenqueue callRef call =
            (callRef, call) <| onHold qs
        in
          case Map.lookup monitorRef (active qs) of
            Nothing -> continue qs { active = withoutRef }
            Just (node, handle, callRef, call) -> do
              result <- wait handle
              case result of
                AsyncDone ret -> do
                  qs' <- assignTasks qs
                    { slaves = Map.adjust (const Nothing) node (slaves qs)
                    , active = withoutRef
                    }
                  replyTo callRef (Just ret)
                  liftIO (MSemN.signal idleSlaves 1)
                  continue qs'
                AsyncPending -> fail "Waited for an async task, but still pending"
                _ -> do
                  say (show node ++ " failed. Reassigning task.")
                  -- (temporarily) blacklist the failing node
                  liftIO (MSemN.signal idleSlaves (-1))
                  qs' <- assignTasks qs
                    { slaves = Map.delete node (slaves qs)
                    , active = withoutRef
                    , onHold = reenqueue callRef call
                    }
                  continue qs'

      onSlaveListChanged
        :: MSemN Int
        -> SchedulerState a
        -> SlaveListChanged
        -> Process (ProcessAction (SchedulerState a))
      onSlaveListChanged idleSlaves qs (SlaveListChanged slaveSet) = do
        liftIO (MSemN.signal idleSlaves delta)
        assignTasks (qs { slaves = slaves qs `Map.union` newSlaves `Map.intersection` newSlaves }) >>= continue
          where
            delta =
              Set.size slaveSet - Map.size (slaves qs)
            newSlaves =
              Map.fromSet (const Nothing) slaveSet

      slaveDiscovery :: ProcessId -> Process ()
      slaveDiscovery queue = forever $ do
        self <- getSelfNode
        slaves <- Set.delete self . Set.fromList <$> P2P.getPeers
        cast queue (SlaveListChanged slaves)
        liftIO (threadDelay 2000000)

{-| Register as a slave node and request tasks from the master node. Blocks.
    Slave discovery is done in a P2P fashion. We have a star topology with the
    master node at the center.
-}
work
  :: String
  -- ^ Host name of the local node, e.g. its IP address.
  -> String
  -- ^ Port number of the local node.
  -> NodeId
  -- ^ @NodeId@ of the master node.
  -> RemoteTable
  -> IO ()
work host service master rt =
  P2P.bootstrap host service [master] rt ping
    where
      ping = do
        liftIO (threadDelay 2000000)
        -- This will try to reach and register with the master node. It's necessary
        -- in case we lost our master.
        -- A little unfortunate that we have to hardcode this...
        whereisRemoteAsync master "P2P:Controller"
        ping
