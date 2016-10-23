{-| The API part of @feed-gipeda@. The console client is just a thin wrapper
    around this.
-}

module FeedGipeda
  ( Endpoint (..)
  , feedGipeda
  , module FeedGipeda.Types
  ) where


import           Control.Arrow                    (second)
import           Control.Concurrent               (forkIO)
import           Control.Concurrent.Chan          (Chan, newChan, readChan,
                                                   writeChan)
import qualified Control.Distributed.Backend.P2P  as P2P
import           Control.Distributed.Process      (Process, RemoteTable, liftIO,
                                                   say, spawnLocal)
import           Control.Distributed.Process.Node (initRemoteTable, runProcess)
import           Control.Logging                  as Logging
import           Control.Monad                    (forever, void, when)
import           Data.List                        (elemIndex)
import           Data.Maybe                       (fromMaybe, isJust)
import           Data.Set                         (Set)
import           Data.Time                        (NominalDiffTime)
import qualified FeedGipeda.Config                as Config
import qualified FeedGipeda.Gipeda                as Gipeda
import           FeedGipeda.GitShell              (SHA)
import qualified FeedGipeda.Master                as Master
import qualified FeedGipeda.Master.CommitQueue    as CommitQueue
import qualified FeedGipeda.Master.File           as Master.File
import           FeedGipeda.Prelude
import           FeedGipeda.Repo                  (Repo)
import qualified FeedGipeda.TaskScheduler         as TaskScheduler
import qualified FeedGipeda.THGenerated           as THGenerated
import           FeedGipeda.Types
import           Network.URI                      (parseURI)
import           System.Directory                 (getAppUserDataDirectory)
import           System.Exit                      (exitSuccess)
import           System.FilePath                  ((</>))


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
    Verbose    -> Logging.setLogLevel Logging.LevelDebug
    NotVerbose -> Logging.setLogLevel Logging.LevelWarn

  case cmd of
    Check ->
      -- Just perform a syntax check on the given configFile
      Config.checkFile (configFile paths) >>= maybe exitSuccess error
    Build mode timeout -> do
      case slaveEndpoint role_ of
        Just (Endpoint shost sport) -> do
          let
            run = if isBoth role_ then void . forkIO else id
            Endpoint mhost mport = masterEndpoint role_
            master = P2P.makeNodeId (mhost ++ ":" ++ show mport)
          run (TaskScheduler.work shost (show sport) master remoteTable)
        _ -> return ()

      case (role_, masterEndpoint role_) of
        (Slave _ _, _) -> return ()
        (_, Endpoint host port) -> do
          queue <- CommitQueue.new
          P2P.bootstrap host (show port) [] remoteTable $ do
            let
              toTask :: (Repo, SHA) -> IO (TaskScheduler.Task String)
              toTask (repo, commit) = do
                script <- Gipeda.determineBenchmarkScript repo
                let closure = THGenerated.benchmarkClosure script repo commit timeout
                let finalize = Master.File.writeBenchmarkCSV repo commit . fromMaybe ""
                return (THGenerated.stringDict, closure, finalize)

            TaskScheduler.start (CommitQueue.dequeue queue >>= toTask)
            liftIO (Master.checkForNewCommits paths deployment mode queue)
