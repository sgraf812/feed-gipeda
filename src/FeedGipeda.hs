{-| The API part of @feed-gipeda@. The console client is just a thin wrapper
    around this.
-}

module FeedGipeda
  ( Endpoint (..)
  , feedGipeda
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
import           Data.Maybe                                         (isJust)
import           Data.Set                                           (Set)
import           Data.Time                                          (NominalDiffTime)
import qualified FeedGipeda.Config                                  as Config
import           FeedGipeda.GitShell                                (SHA)
import qualified FeedGipeda.Master                                  as Master
import           FeedGipeda.Repo                                    (Repo)
import qualified FeedGipeda.TaskQueue                               as TaskQueue
import qualified FeedGipeda.THGenerated                             as THGenerated
import           Network.URI                                        (parseURI)
import           System.Directory                                   (getAppUserDataDirectory)
import           System.Exit                                        (exitSuccess)
import           System.FilePath                                    ((</>))


-- | An IP endpoint, or rather some string and some integer delimited by a colon.
data Endpoint
  = Endpoint
  { host :: String
  , port :: Int
  } deriving (Show, Eq)


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
  :: FilePath -> FilePath
  -> Bool -> Int -> Bool
  -> Maybe String
  -> Maybe Endpoint -> Maybe Endpoint
  -> Bool
  -> IO ()
feedGipeda
  gipeda configFile
  oneShot dt check
  rsyncPath
  master slave
  verbose = do
  if verbose
    then Logging.setLogLevel Logging.LevelDebug
    else Logging.setLogLevel Logging.LevelWarn

  -- Handle the --check flag. Just perform a syntax check on the given configFile
  when check $ Config.checkFile configFile >>= maybe exitSuccess fail

  -- translate the config flags to endpoints
  let
    (masterEndpoint, workerEndpoint) =
      case (master, slave) of
        (Nothing, Nothing) -> (Just (Endpoint "localhost" 1337), Just (Endpoint "localhost" 1338))
        _ -> (master, slave)

  case workerEndpoint of
    Just (Endpoint host port) -> do
      let
        run = if isJust masterEndpoint then void . forkIO else id
      run $ do
        backend <- SLN.initializeBackend host (show port) remoteTable
        TaskQueue.work backend
    _ -> return ()

  case masterEndpoint of
    Nothing -> return ()
    Just (Endpoint host port) -> do
      backend <- SLN.initializeBackend host (show port) remoteTable
      node <- SLN.newLocalNode backend
      tasks <- newChan
      runProcess node $ do
        taskQueue <- TaskQueue.start backend

        spawnLocal $ forever $ do
          (finalize, benchmarkScript, repo, commit) <- liftIO (readChan tasks)
          spawnLocal $ do
            result <- TaskQueue.execute taskQueue
              THGenerated.stringDict
              (THGenerated.benchmarkClosure benchmarkScript repo commit)
            liftIO (finalize result)

        let
          onNewCommit :: (String -> IO ()) -> String -> Repo -> SHA -> IO ()
          onNewCommit finalize benchmarkScript repo commit =
            writeChan tasks (finalize, benchmarkScript, repo, commit)

          paths :: Master.Paths
          paths =
            Master.Paths configFile rsyncPath gipeda

        liftIO $ if oneShot
          then Master.checkForNewCommits paths Master.OneShot onNewCommit
          else Master.checkForNewCommits paths (Master.Watch (fromIntegral dt)) onNewCommit
