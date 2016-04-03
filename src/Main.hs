import           ConcurrentQueueSet       (ConcurrentQueueSet)
import qualified ConcurrentQueueSet
import           Control.Concurrent       (forkIO)
import           Control.Monad            (forever)
import           Data.Set                 (Set)
import           GitShell                 (SHA)
import           Repo                     (Repo)
import qualified RepoWatcher
import           System.Console.ArgParser (Descr (..), ParserSpec, andBy,
                                           optFlag, parsedBy, withParseResult)
import           System.Directory         (getAppUserDataDirectory)
import           System.FilePath          ((</>))
import           Worker                   (WorkItem ())
import qualified Worker


data CmdArgs
  = CmdArgs
  { benchmarkScript :: FilePath
  , gipeda          :: FilePath
  , configFile      :: FilePath
  }


cmdParser :: FilePath -> ParserSpec CmdArgs
cmdParser configFile = CmdArgs
  `parsedBy` optFlag "cloben" "benchmark" `Descr` "Benchmark script which will be"
  ++ " supplied the repository to name and specific commit to benchmark"
  `andBy` optFlag "gipeda" "gipeda" `Descr` "Path to the gipeda executable"
  ++ " the directory of which includes assets such as site/ scaffolding and install-jslibs.sh"
  `andBy` optFlag configFile "config" `Descr` "Path to the YAML file containing"
  ++ " a list of watched repositories. Will be watched for changes."
  ++ " Defaults to the .feed-gipeda/feed-gipeda.yaml sub path under"
  ++ " $HOME resp. %APPDATA%/Roaming/"


main :: IO ()
main = do
  configFile <- getAppUserDataDirectory ("feed-gipeda" </> "feed-gipeda.yaml")
  withParseResult (cmdParser configFile) $ \(CmdArgs cloben gipeda configFile) -> do
    workItems <- ConcurrentQueueSet.empty -- Work item queue between RepoWatcher and Worker

    let
      onNewCommits :: Repo -> Set SHA -> IO ()
      onNewCommits repo commits = do
        mapM_ (ConcurrentQueueSet.enqueue workItems . Worker.Benchmark cloben repo) commits
        ConcurrentQueueSet.enqueue workItems (Worker.Regenerate gipeda repo)

    -- We have to run the benchmark worker on the main thread so that asynchronous
    -- @UserInterrupt@s are handled correspondingly (e.g. by deleting the touched
    -- .log file).

    -- Each time the config file @f@ changes, we @cloneAddedRepos@ (New repos with
    -- new SHAs).
    -- We also touch the config file initially (recognizing local clones and
    -- already benchmarked commits) and check all clones for updated remotes
    -- (old Repos, new SHAs).
    -- New @(Repo, SHA)@ pairs are @WorkItem@s to be pushed to the
    -- worker via the @workItems@ channel.
    forkIO (RepoWatcher.watchConfiguredRepos configFile onNewCommits)
    -- Performs the benchmarking and site generation by calling the appropriate
    -- scripts.
    -- I'm unsure about whether this should be parallelized with multiple workers.
    -- Performance couldn't be compared among builds anymore.
    forever $ ConcurrentQueueSet.dequeue workItems >>= Worker.work
