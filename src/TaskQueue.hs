{-# LANGUAGE DeriveGeneric #-}

module TaskQueue
  ( TaskQueue
  , start
  , execute
  ) where


import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Control.Distributed.Process.Backend.SimpleLocalnet (Backend (..), startSlave)
import Control.Distributed.Process hiding (call)
import Control.Distributed.Process.Serializable
import Control.Distributed.Process.ManagedProcess
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Binary (Binary)
import GitShell (SHA)
import Repo (Repo)
import Control.Distributed.Process.Extras hiding (send, call)
import Control.Distributed.Process.Extras.Time
import Control.Distributed.Process.Async
import Data.Sequence (Seq, (<|), (|>), ViewL (..))
import qualified Data.Sequence as Seq


newtype TaskQueue
  = TaskQueue ProcessId


instance Resolvable TaskQueue where
  resolve (TaskQueue pid) = return (Just pid)


data Task
  = Task Repo SHA
  deriving (Eq, Ord, Show, Generic, Typeable)
instance Binary Task


newtype Result
  = Result String
  deriving (Eq, Ord, Show, Generic, Typeable)
instance Binary Result


newtype SlaveListChanged
  = SlaveListChanged (Set NodeId)
  deriving (Ord, Eq, Show, Generic, Typeable)
instance Binary SlaveListChanged


data QueueState
  = QueueState
  { slaves :: Map NodeId (Maybe MonitorRef)
  , active :: Map MonitorRef (NodeId, Async String, CallRef String, Task)
  , onHold :: Seq (CallRef String, Task)
  }


initialQueueState :: QueueState
initialQueueState =
  QueueState Map.empty Map.empty Seq.empty


start
  :: Backend
  -> Static (SerializableDict String)
  -> ((Repo, SHA) -> Closure (Process String))
  -> Process TaskQueue
start backend dict work = do
  queue <- TaskQueue <$> spawnLocal (queue dict work)
  spawnLocal (slaveDiscovery backend queue)
  return queue


execute
  :: TaskQueue
  -> Repo
  -> SHA
  -> Process String
execute queue repo sha =
  call queue (Task repo sha)


queue :: Static (SerializableDict String) -> ((Repo, SHA) -> Closure (Process String)) -> Process ()
queue dict work = serve () init process
  where
    init :: InitHandler () QueueState
    init () =
      return (InitOk initialQueueState Infinity)

    process :: ProcessDefinition QueueState
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

    assignTasks :: QueueState -> Process QueueState
    assignTasks qs@(QueueState slaves active onHold) = do
      let
        idle :: Set NodeId
        idle =
          Map.keysSet (Map.filter isNothing slaves)

        assignment :: Maybe (NodeId, CallRef String, Task)
        assignment = do
          node <- fst <$> Set.minView idle
          (ref, task) <- case Seq.viewl onHold of
            EmptyL -> Nothing
            head :< _ -> Just head
          return (node, ref, task)

      case assignment of
        Nothing -> return qs
        Just (node, callRef, task@(Task repo sha)) -> do
          handle <- async (remoteTask dict node (work (repo, sha)))
          monitorRef <- monitorAsync handle
          return qs
            { slaves = Map.insert node (Just monitorRef) slaves
            , onHold = Seq.drop 1 onHold
            , active = Map.insert monitorRef (node, handle, callRef, task) active
            }

    onNewTask :: QueueState -> CallRef String -> Task -> Process (ProcessReply String QueueState)
    onNewTask qs ref task =
      assignTasks (qs { onHold = onHold qs |> (ref, task) }) >>= noReply_

    onTaskCompleted :: QueueState -> ProcessMonitorNotification -> Process (ProcessAction QueueState)
    onTaskCompleted qs (ProcessMonitorNotification monitorRef _ _) =
      let
        withoutRef =
          Map.delete monitorRef (active qs)
        withIdleSlave nodeId =
          Map.adjust (const Nothing) nodeId (slaves qs)
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
                  { slaves = withIdleSlave node
                  , active = withoutRef
                  }
                replyTo callRef ret
                continue qs'
              AsyncPending -> fail "Waited for an async task, but still pending"
              _ -> do
                qs' <- assignTasks qs
                  { slaves = withIdleSlave node
                  , active = withoutRef
                  , onHold = reenqueue callRef task
                  }
                continue qs'

    onSlaveListChanged :: QueueState -> SlaveListChanged -> Process (ProcessAction QueueState)
    onSlaveListChanged qs (SlaveListChanged slaveSet) =
      assignTasks (qs { slaves = slaves qs `Map.union` newSlaves `Map.intersection` newSlaves }) >>= continue
        where
          newSlaves =
            Map.fromSet (const Nothing) slaveSet


slaveDiscovery :: Backend -> TaskQueue -> Process ()
slaveDiscovery backend queue = forever $ do
  slaves <- Set.fromList <$> liftIO (findPeers backend 1000000)
  cast queue (SlaveListChanged slaves)
  liftIO $ threadDelay (30 * 1000000)


startWorker :: Backend -> IO ()
startWorker = startSlave
