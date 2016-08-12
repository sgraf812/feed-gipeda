{-| The API part of @feed-gipeda@. The console client is just a thin wrapper
    around this.
-}

module FeedGipeda
  ( Endpoint (..)
  , feedGipeda
  , module FeedGipeda.Types
  ) where


import           Control.Arrow                                      (second)
import           Control.Concurrent                                 (forkIO)
import           Control.Concurrent.Chan                            (Chan,
                                                                     newChan,
                                                                     readChan,
                                                                     writeChan)
import           Control.Distributed.Process                        (Process,
                                                                     RemoteTable,
                                                                     liftIO,
                                                                     say,
                                                                     spawnLocal)
import qualified Control.Distributed.Process.Backend.SimpleLocalnet as SLN
import           Control.Distributed.Process.Node                   (initRemoteTable,
                                                                     runProcess)
import           Control.Logging                                    as Logging
import           Control.Monad                                      (forever,
                                                                     void, when)
import           Data.List                                          (elemIndex)
import           Data.Maybe                                         (fromMaybe,
                                                                     isJust)
import           Data.Set                                           (Set)
import           Data.Time                                          (NominalDiffTime)
import qualified FeedGipeda.Config                                  as Config
import           FeedGipeda.GitShell                                (SHA)
import qualified FeedGipeda.Master                                  as Master
import qualified FeedGipeda.Master.CommitQueue                      as CommitQueue
import           FeedGipeda.Prelude
import           FeedGipeda.Repo                                    (Repo)
import qualified FeedGipeda.TaskScheduler                           as TaskScheduler
import qualified FeedGipeda.THGenerated                             as THGenerated
import           FeedGipeda.Types
import           Network.URI                                        (parseURI)
import           System.Directory                                   (getAppUserDataDirectory)
import           System.Exit                                        (exitSuccess)
import           System.FilePath                                    ((</>))


remoteTable :: RemoteTable
remoteTable =
  THGenerated.__remoteTable initRemoteTable


{-| The parameters correspond exactly to the command line parameters, to
    you should read the @--help@ message for more thorough documentation.

    @feedGipeda@ determines the appropriate mode of operation (e.g. watching or one-shot).
    It also works as master or slave node, depending on which endpoints are given.
    Lastly, @verbose@ will lead to more debug output.
-}
feedGipeda
  :: Paths
  -> Command
  -> Deployment
  -> ProcessRole
  -> Verbosity
  -> IO ()
feedGipeda paths cmd deployment role_ verbosity = do
  case verbosity of
    Verbose -> Logging.setLogLevel Logging.LevelDebug
    NotVerbose -> Logging.setLogLevel Logging.LevelWarn

  case cmd of
    Check ->
      -- Just perform a syntax check on the given configFile
      Config.checkFile (configFile paths) >>= maybe exitSuccess error
    Build mode timeout -> do
      case slaveEndpoint role_ of
        Just (Endpoint host port) -> do
          let
            run = if isBoth role_ then void . forkIO else id
          run $ do
            backend <- SLN.initializeBackend host (show port) remoteTable
            TaskScheduler.work backend
        _ -> return ()

      case masterEndpoint role_ of
        Nothing -> return ()
        Just (Endpoint host port) -> do
          backend <- SLN.initializeBackend host (show port) remoteTable
          node <- SLN.newLocalNode backend
          queue <- CommitQueue.new
          runProcess node $ do
            let
              toTask :: CommitQueue.ScheduledCommit -> TaskScheduler.Task String
              toTask (benchmarkScript, repo, commit, finalize) =
                (THGenerated.stringDict, THGenerated.benchmarkClosure benchmarkScript repo commit timeout, finalize . fromMaybe "")

            TaskScheduler.start backend (toTask <$> CommitQueue.dequeue queue)
            liftIO (Master.checkForNewCommits paths deployment mode queue)
