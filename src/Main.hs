import qualified Config
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
import           Control.Monad                                      (forever,
                                                                     void, when)
import           Data.List                                          (elemIndex)
import           Data.Maybe                                         (isJust)
import           Data.Set                                           (Set)
import           Data.Time                                          (NominalDiffTime)
import           GitShell                                           (SHA)
import           Network.URI                                        (parseURI)
import           Repo                                               (Repo)
import qualified RepoWatcher
import           System.Console.ArgParser                           (Descr (..),
                                                                     ParserSpec,
                                                                     andBy,
                                                                     boolFlag,
                                                                     optFlag,
                                                                     optFlagArgs,
                                                                     parsedBy,
                                                                     withParseResult)
import           System.Directory                                   (getAppUserDataDirectory)
import           System.Exit                                        (exitSuccess)
import           System.FilePath                                    ((</>))
import qualified TaskQueue
import qualified THGenerated
import qualified Worker


data CmdArgs
  = CmdArgs
  { benchmarkScript :: FilePath
  , gipeda          :: FilePath
  , configFile      :: Maybe FilePath
  , fetchInterval   :: Int
  , check           :: Bool
  , rsyncPath       :: Maybe String
  , master          :: Maybe String
  , slave           :: Maybe String
  }


maybeFlag key =
  optFlagArgs Nothing key Nothing (const Just)


cmdParser :: ParserSpec CmdArgs
cmdParser = CmdArgs
  `parsedBy` optFlag "cloben" "benchmark" `Descr` "Benchmark script which will be"
  ++ " supplied the repository to name and specific commit to benchmark"
  `andBy` optFlag "gipeda" "gipeda" `Descr` "Path to the gipeda executable"
  ++ " the directory of which includes assets such as site/ scaffolding and install-jslibs.sh"
  `andBy` maybeFlag "config" `Descr` "Path to the YAML file containing"
  ++ " a list of watched repositories. Will be watched for changes."
  ++ " Defaults to the .feed-gipeda/feed-gipeda.yaml sub path under"
  ++ " $HOME resp. %APPDATA%/Roaming/"
  `andBy` optFlag (60*60) "dt" `Descr` "Fetch interval for all repos in seconds."
  ++ " Defaults to 60*60 ~= 1 hour"
  `andBy` boolFlag "check" `Descr` "Verify that the given config file is well-formed and exit"
  `andBy` maybeFlag "rsync" `Descr` "ssh path under which to deploy site/ folders"
  ++ " with rsync"
  `andBy` maybeFlag "master" `Descr` "Start in master mode, distributing work items."
  ++ " Identified via the given TCP endpoint (ipadress:portnumber)."
  `andBy` maybeFlag "slave" `Descr` "Start in slave mode, requesting work items from a master node."
  ++ " Identified via the given TCP endpoint (ipadress:portnumber)."


parseEndpoint :: String -> IO (String, String)
parseEndpoint s =
  case elemIndex ':' s of
    Just idx -> return (second tail (splitAt idx s))
    Nothing -> fail ("Could not parse endpoint " ++ s)


remoteTable :: RemoteTable
remoteTable =
  THGenerated.__remoteTable initRemoteTable


main :: IO ()
main = withParseResult cmdParser $
  \(CmdArgs cloben gipeda configFile' dt check rsyncPath master slave) -> do
    configFile <- maybe
      (getAppUserDataDirectory ("feed-gipeda" </> "feed-gipeda.yaml"))
      return
      configFile'
    -- Handle the --check flag. Just perform a syntax check on the given configFile
    when check $ Config.checkFile configFile >>= maybe exitSuccess fail

    master' <- maybe (return Nothing) (fmap Just . parseEndpoint) master
    slave' <- maybe (return Nothing) (fmap Just . parseEndpoint) slave

    -- translate the config flags to endpoints
    (masterEndpoint, workerEndpoint) <- case (master', slave') of
      (Nothing, Nothing) -> return (Just ("localhost", "1337"), Just ("localhost", "1338"))
      _ -> return (master', slave')

    case workerEndpoint of
      Just (host, port) -> do
        let
          run = if isJust masterEndpoint then void . forkIO else id
        run $ do
          backend <- SLN.initializeBackend host port remoteTable
          TaskQueue.work backend
      _ -> return ()

    case masterEndpoint of
      Nothing -> return ()
      Just (host, port) -> do
        backend <- SLN.initializeBackend host port remoteTable
        node <- SLN.newLocalNode backend
        runProcess node $ do
          taskQueue <- TaskQueue.start backend

          -- Performs the benchmarking and site generation by calling the appropriate
          -- scripts in a separate process, listening for new repos and commits
          -- though a channel. That's necessary because RepoWatcher's NewCommitsAction
          -- has IO () in negative position (as opposed to Process ())
          deferredEvents <- liftIO newChan
          spawnLocal $ forever $ do
            (repo, commit) <- liftIO (readChan deferredEvents)
            spawnLocal $ do
              result <- TaskQueue.execute taskQueue
                THGenerated.stringDict
                (THGenerated.benchmarkClosure (cloben, repo, commit))
              liftIO (Worker.finalize gipeda rsyncPath repo commit result)

          -- Each time the config file @f@ changes, we @cloneAddedRepos@ (New repos with
          -- new SHAs).
          -- We also touch the config file initially (recognizing local clones and
          -- already benchmarked commits) and check all clones for updated remotes
          -- (old Repos, new SHAs).
          -- New @(Repo, SHA)@ pairs notify the @onNewCommits@ handler, which will
          -- push @WorkItem@s along the @workItems@ queue to the worker.
          let
            onNewCommits :: Repo -> Set SHA -> IO ()
            onNewCommits repo =
              mapM_ (\commit -> writeChan deferredEvents (repo, commit))

          liftIO (RepoWatcher.watchConfiguredRepos configFile (fromIntegral dt) onNewCommits)
