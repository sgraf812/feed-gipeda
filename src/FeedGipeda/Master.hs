{-| The master node works as follows:

    1. Maintain updated local clones of a number of configured repositories
    2. Ask @gipeda@ which of those repositories have commits that need to be benchmarked
    3. Notify the caller for each (Repo, SHA) pair (to delegate work to slaves)
    4. Call @gipeda@ when either the repository or some benchmark result file changed

    If in @Watch@ mode (as opposed to @OneShot@ mode), the configuration file is
    watched for updates to the actively watched repositories, as well as
    fetches all currently watched repositories at a fixed interval. Existing
    clones are detected and reused, so that restarting the daemon will not do
    unnecessary work.
-}

module FeedGipeda.Master
  ( NewCommitAction
  , checkForNewCommits
  ) where


import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Concurrent.MVar    (MVar, newEmptyMVar, putMVar,
                                             readMVar)
import           Control.Logging            as Logging
import           Control.Monad              (forM_, forever, when)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Functor
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe, listToMaybe)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text
import           Data.Time                  (NominalDiffTime)
import qualified Data.Time                  as Time
import           Debug.Trace                (traceShowId)
import qualified FeedGipeda.Config          as Config
import qualified FeedGipeda.Gipeda          as Gipeda
import           FeedGipeda.GitShell        (SHA)
import qualified FeedGipeda.GitShell        as GitShell
import qualified FeedGipeda.Master.File     as File
import qualified FeedGipeda.Master.Finalize as Finalize
import           FeedGipeda.Master.RepoDiff (RepoDiff)
import qualified FeedGipeda.Master.RepoDiff as RepoDiff
import           FeedGipeda.Repo            (Repo)
import qualified FeedGipeda.Repo            as Repo
import           FeedGipeda.Types
import           Reactive.Banana            ((<@), (<@>))
import qualified Reactive.Banana            as Banana
import qualified Reactive.Banana.Frameworks as Banana
import           System.Directory           (canonicalizePath,
                                             getCurrentDirectory)
import           System.FilePath            (equalFilePath, takeDirectory)
import qualified System.FSNotify            as FS


-- | Handler which will be called for commits @gipeda@ requests to benchmark.
type NewCommitAction
  = (String -> IO ())
  -- ^ Continuation to call with the benchmark results
  -> String
  -- ^ The @benchmarkScript@ as determined when assembling the @gipeda.yaml@
  -> Repo
  -- ^ The repository of the commit to benchmark
  -> SHA
  -- ^ The commit to benchmark
  -> IO ()


notifyOnNewCommitsInBacklog :: NewCommitAction -> (Repo, Set SHA) -> IO ()
notifyOnNewCommitsInBacklog onNewCommit (repo, backlog) = do
  benchmarkScript <- Gipeda.determineBenchmarkScript repo
  forM_ backlog $ \commit ->
    onNewCommit (File.writeBenchmarkCSV repo commit) benchmarkScript repo commit


finalizeRepos :: Paths -> Deployment -> Set Repo -> Set Repo -> IO ()
finalizeRepos paths deployment activeRepos repos = forM_ repos $ \repo -> do
  Finalize.regenerateAndDeploy (gipeda paths) deployment activeRepos repo
  File.writeBacklog repo


readConfigFileRepos :: FS.Event -> IO (Maybe (Set Repo))
readConfigFileRepos evt =
  case evt of
    FS.Removed _ _ -> return (Just Set.empty)
    _ ->
      Config.decodeFile (FS.eventPath evt) >>= either
        (\err -> Logging.warn (Text.pack err) >> return Nothing)
        (return . Just . Config.repos)


accumDiff
  :: Banana.MonadMoment moment
  => Banana.Event (Set Repo)
  -> moment (Banana.Event RepoDiff)
accumDiff repos =
  fst <$> Banana.mapAccum Set.empty ((\new old -> (RepoDiff.compute old new, new)) <$> repos)


dedupCommitsAndNotifyWhenEmpty
  :: IO ()
  -> Banana.Event (Repo, Set SHA)
  -> Banana.MomentIO (Banana.Event (Repo, Set SHA))
dedupCommitsAndNotifyWhenEmpty notify commits = do
  (events, maps) <- Banana.mapAccum Map.empty (filterDuplicates <$> commits)
  Banana.mapEventIO id events
    where
      filterDuplicates
        :: (Repo, Set SHA)
        -> Map Repo (Set SHA)
        -> (IO (Repo, Set SHA), Map Repo (Set SHA))
      filterDuplicates (repo, commits) inProgress =
        let
          nonDuplicates =
            Set.difference commits (fromMaybe Set.empty (Map.lookup repo inProgress))

          newMap =
            if Set.null commits
              then Map.delete repo inProgress
              else Map.insert repo commits inProgress

          eventAction = do
            when (Map.null newMap) notify
            Logging.log (Text.pack ("Backlog for " ++ Repo.uri repo
              ++ " contained " ++ show (Set.size commits) ++ " commits, "
              ++ show (Set.size nonDuplicates) ++ " unhandled."))
            return (repo, nonDuplicates)
        in
          (eventAction, newMap)


periodically :: NominalDiffTime -> Banana.MomentIO (Banana.Event ())
periodically dt = do
  (event, fire) <- Banana.newEvent
  liftIO $ forkIO $ forever $ do
    begin <- Time.getCurrentTime
    fire ()
    end <- Time.getCurrentTime
    let elapsed = Time.diffUTCTime end begin
    threadDelay (ceiling ((dt - elapsed) * 1000000))
  return event


singleShot :: MVar () -> Banana.MomentIO (Banana.Event ())
singleShot mvar = do
  (event, fire) <- Banana.newEvent
  liftIO $ forkIO $ readMVar mvar >>= fire
  return event


repoOfFileEvent
  :: FilePath
  -> Banana.Behavior (Set Repo)
  -> Banana.Event FS.Event
  -> Banana.MomentIO (Banana.Event Repo)
repoOfFileEvent cwd activeRepos fileEvents =
  Banana.filterJust <$> Banana.mapEventIO
    id
    (File.repoOfPath cwd <$> activeRepos <@> (FS.eventPath <$> fileEvents))


{-| See the module docs. This function builds up the FRP network with primitives
    from @reactive-banana@. No other module should be 'tainted' by that.
-}
checkForNewCommits
  :: Paths
  -> Deployment
  -> BuildMode
  -> NewCommitAction
  -> IO ()
checkForNewCommits paths deployment mode onNewCommit = FS.withManager $ \mgr -> do
  cwd <- getCurrentDirectory
  exit <- newEmptyMVar
  start <- newEmptyMVar

  let
    watchFile :: FilePath -> Banana.MomentIO (Banana.Event FS.Event)
    watchFile path' = do
      (event, fire) <- Banana.newEvent
      path <- liftIO (canonicalizePath path')
      liftIO $ FS.watchDir mgr (takeDirectory path) (equalFilePath path . FS.eventPath) $ \evt -> do
        Logging.debug (Text.pack ("File changed: " ++ show evt))
        fire evt
      return event

    watchTree :: FilePath -> (FilePath -> Bool) -> Banana.MomentIO (Banana.Event FS.Event)
    watchTree path predicate = do
      (event, fire) <- Banana.newEvent
      liftIO $ FS.watchTree mgr path (predicate . FS.eventPath) $ \evt -> do
        Logging.debug (Text.pack ("File changed: " ++ show evt))
        fire evt
      return event

    networkDescription :: Banana.MomentIO ()
    networkDescription = do
      -- Source: Initial tick to read in the file
      initialConfig <- (FS.Added (configFile paths) undefined <$) <$> singleShot start

      -- Source: Events resulting from watching the config file
      configFileChanges <-
        case mode of
          Once -> return initialConfig
          WatchForChanges _ -> Banana.unionWith const initialConfig <$> watchFile (configFile paths)

      activeRepos <- Banana.filterJust <$> Banana.mapEventIO readConfigFileRepos configFileChanges
      activeReposB <- Banana.stepper Set.empty activeRepos
      diffsWithoutRefresh <- accumDiff activeRepos

      -- Source: When in PeriodicRefresh mode, occasionally mark all repos dirty
      diffs <-
        case mode of
          Once -> return diffsWithoutRefresh
          WatchForChanges dt -> do
            ticks <- periodically dt
            return (Banana.unionWith const (RepoDiff.compute Set.empty <$> activeReposB <@ ticks) diffsWithoutRefresh)

      -- Fetch every added ('dirty') repository, delay until fetch is complete
      -- TODO: parallelize and/or get rid of mapM_ somehow
      fetchedRepos <-
        Banana.mapEventIO
          (\added -> do
            forM_ added $ \repo -> do
              Logging.log (Text.pack ("Syncing " ++ Repo.shortName repo))
              GitShell.sync repo
            return added)
          (RepoDiff.added <$> diffs)

      -- Source: Changed benchmark CSV files
      benchmarks <- watchTree cwd (File.isBenchmarkCSV cwd)
      benchmarkedRepos <- repoOfFileEvent cwd activeReposB benchmarks

      -- Sink: produce the appropriate backlog and deploy
      let reposToFinish = Banana.unionWith Set.union fetchedRepos (Set.singleton <$> benchmarkedRepos)
      Banana.reactimate (finalizeRepos paths deployment <$> activeReposB <@> reposToFinish)

      -- Source: Backlog changes
      backlogs <- watchTree cwd (File.isBacklog cwd)
      backlogRepos <- repoOfFileEvent cwd activeReposB backlogs

      -- Sink: Backlog changes kick off workers, resp. the new commit action
      backlogCommits <- Banana.mapEventIO (\repo -> (,) repo <$> File.readBacklog repo) backlogRepos
      let doExit = when (mode == Once) (putMVar exit ())
      dedupedCommits <- dedupCommitsAndNotifyWhenEmpty doExit backlogCommits
      Banana.reactimate (notifyOnNewCommitsInBacklog onNewCommit <$> dedupedCommits)

  network <- Banana.compile networkDescription
  Banana.actuate network
  putMVar start ()
  readMVar exit
