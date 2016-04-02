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
import           System.Directory         (canonicalizePath)
import           Worker                   (WorkItem ())
import qualified Worker


data CmdArgs
  = CmdArgs
  { benchmarkScript :: FilePath
  , gipeda          :: FilePath
  }


cmdParser :: ParserSpec CmdArgs
cmdParser = CmdArgs
  `parsedBy` optFlag "cloben" "benchmark" `Descr` "Benchmark script which will be"
  ++ " supplied the repository to name and specific commit to benchmark"
  `andBy` optFlag "gipeda" "gipeda" `Descr` "Path to the gipeda installation"
  ++ " including assets such as site/ scaffolding and install-jslibs.sh"


configFile :: IO FilePath
configFile =
  canonicalizePath "feed-gipeda.yaml" -- TODO: this is a case for cmd args


main :: IO ()
main = withParseResult cmdParser $ \(CmdArgs cloben gipeda) -> do
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
  f <- configFile
  forkIO (RepoWatcher.watchConfiguredRepos f onNewCommits)
  -- Performs the benchmarking and site generation by calling the appropriate
  -- scripts.
  -- I'm unsure about whether this should be parallelized with multiple workers.
  -- Performance couldn't be compared among builds anymore.
  forever $ ConcurrentQueueSet.dequeue workItems >>= Worker.work
