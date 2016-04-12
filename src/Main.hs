import qualified Config
import           Control.Concurrent       (forkIO)
import           Control.Concurrent.Chan  (Chan, newChan, readChan, writeChan)
import           Control.Monad            (forever, void, when)
import           Data.Set                 (Set)
import           Data.Time                (NominalDiffTime)
import qualified Distributed
import           GitShell                 (SHA)
import           Network.URI              (parseURI)
import           Repo                     (Repo)
import qualified RepoWatcher
import           System.Console.ArgParser (Descr (..), ParserSpec, andBy,
                                           boolFlag, optFlag, optFlagArgs,
                                           parsedBy, withParseResult)
import           System.Directory         (getAppUserDataDirectory)
import           System.Exit              (exitSuccess)
import           System.FilePath          ((</>))
import qualified Worker


data CmdArgs
  = CmdArgs
  { benchmarkScript :: FilePath
  , gipeda          :: FilePath
  , configFile      :: Maybe FilePath
  , fetchInterval   :: Int
  , check           :: Bool
  , rsyncPath       :: Maybe String
  , master          :: Maybe Int
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
  `andBy` maybeFlag "master" `Descr` "Start in master mode, distributing work items at"
  ++ " the given TCP port number."
  `andBy` maybeFlag "slave" `Descr` "Start in slave mode, requesting work items from"
  ++ " the given TCP endpoint (ipadress:portnumber)."


main :: IO ()
main = withParseResult cmdParser $
  \(CmdArgs cloben gipeda configFile' dt check rsyncPath master slave) -> do
    configFile <- maybe
      (getAppUserDataDirectory ("feed-gipeda" </> "feed-gipeda.yaml"))
      return
      configFile'
    -- Handle the --check flag. Just perform a syntax check on the given configFile
    when check $ Config.checkFile configFile >>= maybe exitSuccess fail

    -- translate the config flags to an endpoint
    (endpoint, broker, work) <- case (master, slave) of
      (Nothing, Nothing) -> return ("ipc://feed-gipeda.ipc", True, True)
      (Just port, Nothing) -> return ("tcp://*:" ++ show port, True, False)
      (Nothing, Just endpoint) -> return ("tcp://" ++ endpoint, False, True)
      _ -> fail "Cannot operate in both master and slave mode. Launch as two different processes instead."

    if not broker
      then Distributed.worker endpoint (Worker.benchmark cloben)
      else Distributed.withBroker endpoint (Worker.finalize gipeda rsyncPath) $ \send -> do
        -- Performs the benchmarking and site generation by calling the appropriate
        -- scripts.
        -- I'm unsure about whether this should be parallelized with multiple workers.
        -- Performance couldn't be compared among builds anymore.
        (when work . void . forkIO . Distributed.worker endpoint) (Worker.benchmark cloben)
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
            mapM_ (send repo)

        RepoWatcher.watchConfiguredRepos configFile (fromIntegral dt) onNewCommits
