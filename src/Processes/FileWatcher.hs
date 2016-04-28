{-# LANGUAGE DeriveGeneric #-}

module Processes.FileWatcher
  ( Event (..)
  , eventPath
  , watchDir
  , watchTree
  , watchFile
  ) where


import           Control.Concurrent.Chan                    (Chan, newChan,
                                                             readChan,
                                                             writeChan)
import           Control.Concurrent.MVar                    (MVar, newEmptyMVar,
                                                             putMVar, readMVar)
import qualified Control.Distributed.Process                as DP
import qualified Control.Distributed.Process.Extras         as DPE
import qualified Control.Distributed.Process.ManagedProcess as MP
import           Control.Monad                              (forever)
import           Data.Binary                                (Binary)
import           Data.Typeable                              (Typeable)
import           GHC.Generics                               (Generic)
import           System.FilePath                            (dropFileName,
                                                             equalFilePath)
import qualified System.FSNotify                            as FS


-- exactly the FS.Event data type, but this will make the dependency transparent
-- Also this way we can derive the neccessary instances.
data Event
  = Added FilePath
  | Removed FilePath
  | Modified FilePath
  deriving (Eq, Show, Typeable, Generic)
instance Binary Event


eventPath :: Event -> FilePath
eventPath evt =
  case evt of
    Added path -> path
    Modified path -> path
    Removed path -> path


fromFSEvent :: FS.Event -> Event
fromFSEvent evt =
  case evt of
    FS.Added path _ -> Added path
    FS.Modified path _ -> Modified path
    FS.Removed path _ -> Removed path


type FSWatchChan =
  FS.WatchManager -> FilePath -> (FS.Event -> Bool) -> FS.EventChannel -> IO (IO ())


watchImpl :: FSWatchChan -> FilePath -> (FilePath -> Bool) -> MP.ControlPort Event -> DP.Process ()
watchImpl fswatch path predicate outport = do
  events <- DP.liftIO newChan

  let
    forwardEvents :: DP.Process ()
    forwardEvents = forever $ do
      event <- DP.liftIO (readChan events)
      MP.sendControlMessage outport (fromFSEvent event)

    watch :: DP.Process ()
    watch =
      DP.liftIO $ FS.withManager $ \mgr -> do
        stop <- fswatch
          mgr
          path
          (predicate . FS.eventPath)
          events
        newEmptyMVar >>= readMVar -- Don't know what else to do to block the thread

  pid <- DP.getSelfPid
  DP.spawnLocal (DP.link pid >> forwardEvents)
  watch


watchDir :: FilePath -> (FilePath -> Bool) -> MP.ControlPort Event -> DP.Process ()
watchDir =
  watchImpl FS.watchDirChan


watchTree :: FilePath -> (FilePath -> Bool) -> MP.ControlPort Event -> DP.Process ()
watchTree =
  watchImpl FS.watchTreeChan


watchFile :: FilePath -> MP.ControlPort Event -> DP.Process ()
watchFile path =
  watchDir (dropFileName path) (equalFilePath path)
