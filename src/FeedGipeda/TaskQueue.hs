{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FeedGipeda.TaskQueue
  ( TaskQueue
  , start
  , execute
  , work
  ) where


import           Control.Concurrent                                 (threadDelay)
import           Control.Distributed.Process                        hiding
                                                                     (call)
import           Control.Distributed.Process.Async
import           Control.Distributed.Process.Backend.SimpleLocalnet (Backend (..),
                                                                     findSlaves,
                                                                     startSlave)
import           Control.Distributed.Process.Extras                 hiding
                                                                     (call,
                                                                     send)
import           Control.Distributed.Process.Extras.Time
import           Control.Distributed.Process.ManagedProcess
import           Control.Distributed.Process.Serializable
import           Control.Monad                                      (forever)
import           Data.Binary                                        (Binary)
import           Data.Map                                           (Map)
import qualified Data.Map                                           as Map
import           Data.Maybe                                         (isNothing)
import           Data.Proxy
import           Data.Sequence                                      (Seq,
                                                                     ViewL (..),
                                                                     (<|), (|>))
import qualified Data.Sequence                                      as Seq
import           Data.Set                                           (Set)
import qualified Data.Set                                           as Set
import           Data.Typeable                                      (Typeable)
import           GHC.Generics                                       (Generic)


newtype TaskQueue a
  = TaskQueue ProcessId


instance Resolvable (TaskQueue a) where
  resolve (TaskQueue pid) = return (Just pid)


type Task a
  = (Static (SerializableDict a), Closure (Process a))


newtype Result a
  = Result a
  deriving (Eq, Ord, Show, Generic, Typeable)
instance Binary a => Binary (Result a)


newtype SlaveListChanged
  = SlaveListChanged (Set NodeId)
  deriving (Ord, Eq, Show, Generic, Typeable)
instance Binary SlaveListChanged


data QueueState a
  = QueueState
  { slaves :: Map NodeId (Maybe MonitorRef)
  , active :: Map MonitorRef (NodeId, Async a, CallRef a, Task a)
  , onHold :: Seq (CallRef a, Task a)
  }


initialQueueState :: QueueState a
initialQueueState =
  QueueState Map.empty Map.empty Seq.empty


start
  :: forall a . Serializable a
  => Backend
  -> Process (TaskQueue a)
start backend = do
  queue <- TaskQueue <$> spawnLocal (queue (Proxy :: Proxy a))
  spawnLocal (slaveDiscovery backend queue)
  return queue


execute
  :: Serializable a
  => TaskQueue a
  -> Static (SerializableDict a)
  -> Closure (Process a)
  -> Process a
execute queue dict closure =
  call queue (dict, closure)


queue
  :: forall a . Serializable a
  => Proxy a
  -> Process ()
queue _ = serve () init process
  where
    init :: InitHandler () (QueueState a)
    init () =
      return (InitOk initialQueueState Infinity)

    process :: ProcessDefinition (QueueState a)
    process = defaultProcess
      { apiHandlers =
          [ handleCast onSlaveListChanged
          , handleCallFrom onNewTask
          ]
      , infoHandlers =
          [ handleInfo onTaskCompleted
          ]
      , unhandledMessagePolicy = Log
      }

    assignTasks :: QueueState a -> Process (QueueState a)
    assignTasks qs@(QueueState slaves active onHold) = do
      let
        idle :: Set NodeId
        idle =
          Map.keysSet (Map.filter isNothing slaves)

        assignment :: Maybe (NodeId, CallRef a, Task a)
        assignment = do
          node <- fst <$> Set.minView idle
          (ref, task) <- case Seq.viewl onHold of
            EmptyL -> Nothing
            head :< _ -> Just head
          return (node, ref, task)

      case assignment of
        Nothing -> return qs
        Just (node, callRef, (dict, closure)) -> do
          --say $ "Assigning node " ++ show node ++ " to a task"
          handle <- async (remoteTask dict node closure)
          monitorRef <- monitorAsync handle
          return qs
            { slaves = Map.insert node (Just monitorRef) slaves
            , onHold = Seq.drop 1 onHold
            , active = Map.insert monitorRef (node, handle, callRef, (dict, closure)) active
            }

    onNewTask
      :: QueueState a
      -> CallRef a
      -> Task a
      -> Process (ProcessReply a (QueueState a))
    onNewTask qs ref task =
      assignTasks (qs { onHold = onHold qs |> (ref, task) }) >>= noReply_

    onTaskCompleted
      :: QueueState a
      -> ProcessMonitorNotification
      -> Process (ProcessAction (QueueState a))
    onTaskCompleted qs (ProcessMonitorNotification monitorRef _ _) =
      let
        withoutRef =
          Map.delete monitorRef (active qs)
        reenqueue callRef task =
          (callRef, task) <| onHold qs
      in
        case Map.lookup monitorRef (active qs) of
          Nothing -> continue qs { active = withoutRef }
          Just (node, handle, callRef, task) -> do
            result <- wait handle
            case result of
              AsyncDone ret -> do
                qs' <- assignTasks qs
                  { slaves = Map.adjust (const Nothing) node (slaves qs)
                  , active = withoutRef
                  }
                replyTo callRef ret
                continue qs'
              AsyncPending -> fail "Waited for an async task, but still pending"
              _ -> do
                say (show node ++ " failed. Reassigning task.")
                qs' <- assignTasks qs
                  { slaves = Map.delete node (slaves qs) -- temporarily blacklist the failing node
                  , active = withoutRef
                  , onHold = reenqueue callRef task
                  }
                continue qs'

    onSlaveListChanged
      :: QueueState a
      -> SlaveListChanged
      -> Process (ProcessAction (QueueState a))
    onSlaveListChanged qs (SlaveListChanged slaveSet) =
      assignTasks (qs { slaves = slaves qs `Map.union` newSlaves `Map.intersection` newSlaves }) >>= continue
        where
          newSlaves =
            Map.fromSet (const Nothing) slaveSet


slaveDiscovery :: Backend -> TaskQueue a -> Process ()
slaveDiscovery backend queue = forever $ do
  self <- getSelfNode
  slaves <- Set.fromList . map processNodeId <$> findSlaves backend
  --say $ show slaves
  cast queue (SlaveListChanged (Set.delete self slaves))


work :: Backend -> IO ()
work = startSlave
