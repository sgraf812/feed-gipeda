{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes    #-}

module Distributed
  ( worker
  , withBroker
  ) where


import           Control.Concurrent.Chan (newChan, readChan, writeChan)
import           Control.Concurrent.MVar (modifyMVar, modifyMVar_, newMVar)
import           Control.Monad           (forever)
import           Data.Binary             (Binary (..), decode, encode)
import qualified Data.ByteString.Lazy    as LBS
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           GHC.Generics            (Generic)
import           GitShell                (SHA)
import           Repo                    (Repo)
import           System.ZMQ4.Monadic


data WorkerToBrokerMessage
  = Ready
  | Return Repo SHA String
  deriving (Eq, Ord, Show, Generic)


instance Binary WorkerToBrokerMessage


data BrokerToWorkerMessage
  = Work Repo SHA
  deriving (Eq, Ord, Show, Generic)


instance Binary BrokerToWorkerMessage


receiveDeserialized :: (Receiver t, Binary a) => Socket z t -> ZMQ z a
receiveDeserialized sock =
  decode . LBS.fromStrict <$> receive sock


sendSerialized :: (Sender t, Binary a) => Socket z t -> a -> ZMQ z ()
sendSerialized sock =
  send sock [] . mconcat . LBS.toChunks . encode


withBroker
  :: String
  -> (Repo -> SHA -> String -> IO ())
  -> ((Repo -> SHA -> IO ()) -> IO a)
  -> IO a
withBroker endpoint finalizer inner = runZMQ $ do
  idle <- liftIO newChan
  busy <- liftIO (newMVar Set.empty)
  workItems <- liftIO newChan

  sock <- socket Router
  bind sock endpoint

  -- Receive on another thread.
  async $ forever $ do
    -- The first frame contains just the peer identity (specific to router)
    identity <- receive sock
    -- The rest is actual payload. We don't aim for compatibility with REQ
    -- sockets, so ommitting the frame delimiter should be OK.

    liftIO $ modifyMVar_ busy $ \b -> do
      writeChan idle identity
      return (Set.delete identity b)

    msg <- receiveDeserialized sock
    case msg of
      Ready -> liftIO $ putStrLn "New worker"
      Return repo sha result -> liftIO (finalizer repo sha result)

  -- Also actually send workitems on another thread. Otherwise, the ZMQ context
  -- Would leak and we couldn't use any code with IO in negative position.
  async $ forever $ do
    work <- liftIO (readChan workItems)
    lruIdleWorker <- liftIO (readChan idle)
    liftIO (modifyMVar_ busy (return . Set.insert lruIdleWorker))

    send sock [SendMore] lruIdleWorker -- the identity of the next free peer
    sendSerialized sock work

  liftIO (inner (\repo commit -> writeChan workItems (Work repo commit)))


worker :: String -> (Repo -> SHA -> IO String) -> IO ()
worker endpoint work = runZMQ $ do
  sock <- socket Dealer
  connect sock endpoint

  sendSerialized sock Ready

  forever $ do
    (Work repo commit) <- receiveDeserialized sock
    result <- liftIO (work repo commit)
    sendSerialized sock (Return repo commit result)
