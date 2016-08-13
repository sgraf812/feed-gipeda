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
  ( checkForNewCommits
  ) where


import           Control.Arrow                 (second)
import           Control.Concurrent            (forkIO, threadDelay)
import           Control.Concurrent.Async      (mapConcurrently)
import           Control.Concurrent.Event      (Event)
import qualified Control.Concurrent.Event      as Event
import           Control.Concurrent.Lock       (Lock)
import qualified Control.Concurrent.Lock       as Lock
import           Control.Logging               as Logging
import           Control.Monad                 (foldM, forM_, forever, when)
import           Control.Monad.IO.Class        (liftIO)
import           Data.Functor
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromMaybe, listToMaybe)
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text
import           Data.Time                     (NominalDiffTime, UTCTime)
import qualified Data.Time                     as Time
import           Debug.Trace                   (traceShowId)
import qualified FeedGipeda.Config             as Config
import qualified FeedGipeda.Gipeda             as Gipeda
import           FeedGipeda.GitShell           (SHA)
import qualified FeedGipeda.GitShell           as GitShell
import           FeedGipeda.Master.CommitQueue (CommitQueue)
import qualified FeedGipeda.Master.CommitQueue as CommitQueue
import qualified FeedGipeda.Master.File        as File
import qualified FeedGipeda.Master.Finalize    as Finalize
import           FeedGipeda.Master.RepoDiff    (RepoDiff)
import qualified FeedGipeda.Master.RepoDiff    as RepoDiff
import           FeedGipeda.Prelude
import           FeedGipeda.Repo               (Repo)
import qualified FeedGipeda.Repo               as Repo
import           FeedGipeda.Types
import           Reactive.Banana               ((<@), (<@>))
import qualified Reactive.Banana               as Banana
import qualified Reactive.Banana.Frameworks    as Banana
import           System.Directory              (canonicalizePath,
                                                getCurrentDirectory)
import           System.FilePath               (equalFilePath, takeDirectory)
import qualified System.FSNotify               as FS


finalizeRepos :: Lock -> Paths -> Deployment -> Set Repo -> (UTCTime, Set Repo) -> Map Repo UTCTime -> IO (Map Repo UTCTime)
finalizeRepos lock paths deployment activeRepos (timestamp, repos) lastGenerated =
  foldM finalizeRepo lastGenerated (Set.toList repos)
    where
      finalizeRepo lastGenerated repo = Lock.with lock $
        case Map.lookup repo lastGenerated of
          Just lg | lg > timestamp -> return lastGenerated
          _ -> do
            newLG <- Time.getCurrentTime
            -- TODO: parallelize the gipeda step
            Finalize.regenerateAndDeploy (gipeda paths) deployment activeRepos repo
            return (Map.insert repo newLG lastGenerated)




readConfigFileRepos :: FS.Event -> IO (Maybe (Set Repo))
readConfigFileRepos (FS.Removed _ _) = return (Just Set.empty)
readConfigFileRepos evt =
  Config.decodeFile (FS.eventPath evt) >>= either
    (\err -> logWarn err >> return Nothing)
    (return . Just . Config.repos)


accumDiff
  :: Banana.Event (Set Repo)
  -> Banana.MomentIO (Banana.Event RepoDiff)
accumDiff repos =
  fst <$> Banana.mapAccum Set.empty ((\new old -> (RepoDiff.compute old new, new)) <$> repos)


updateCommitQueue :: CommitQueue -> Repo -> IO Bool
updateCommitQueue queue repo = do
  backlog <- File.readBacklog repo
  CommitQueue.updateRepoBacklog queue repo backlog


fetchRepos :: Set Repo -> IO (UTCTime, Set Repo)
fetchRepos repos = do
  mapConcurrently fetch (Set.toList repos)
  timestamp <- Time.getCurrentTime -- 'last modified'
  return (timestamp, repos)
    where
      fetch repo = do
        logInfo ("Syncing " ++ Repo.shortName repo)
        GitShell.sync repo


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


singleShot :: Event -> Banana.MomentIO (Banana.Event ())
singleShot evt = do
  (event, fire) <- Banana.newEvent
  liftIO $ forkIO $ Event.wait evt >>= fire
  return event


repoOfFileEvent
  :: FilePath
  -> Banana.Behavior (Set Repo)
  -> Banana.Event FS.Event
  -> Banana.MomentIO (Banana.Event (UTCTime, Repo))
repoOfFileEvent cwd activeRepos fileEvents =
  Banana.filterJust <$> Banana.mapEventIO id timestampedRepos
    where
      timestampedRepos =
        reverseRoute <$> activeRepos <@> fileEvents
      reverseRoute repos evt =
        ((,) (FS.eventTime evt) <$>) <$> File.repoOfPath cwd repos (FS.eventPath evt)


accumEM
  :: (Monad m, Banana.MonadMoment mom)
  => a
  -> Banana.Event (a -> m a)
  -> mom (Banana.Event (m a))
accumEM acc fs = Banana.accumE (return acc) ((=<<) <$> fs)


{-| See the module docs. This function builds up the FRP network with primitives
    from @reactive-banana@. No other module should be 'tainted' by that.
-}
checkForNewCommits
  :: Paths
  -> Deployment
  -> BuildMode
  -> CommitQueue
  -> IO ()
checkForNewCommits paths deployment mode commitQueue = FS.withManager $ \mgr -> do
  cwd <- getCurrentDirectory
  exit <- Event.new
  start <- Event.new

  let
    watchFile :: FilePath -> Banana.MomentIO (Banana.Event FS.Event)
    watchFile path' = do
      (event, fire) <- Banana.newEvent
      path <- liftIO (canonicalizePath path')
      liftIO $ FS.watchDir mgr (takeDirectory path) (equalFilePath path . FS.eventPath) $ \evt -> do
        logDebug ("File changed: " ++ show evt)
        fire evt
      return event

    watchTree :: FilePath -> (FilePath -> Bool) -> Banana.MomentIO (Banana.Event FS.Event)
    watchTree path predicate = do
      (event, fire) <- Banana.newEvent
      liftIO $ FS.watchTree mgr path (predicate . FS.eventPath) $ \evt -> do
        logDebug ("File changed: " ++ show evt)
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
      fetchedRepos <- Banana.mapEventIO fetchRepos (RepoDiff.added <$> diffs)

      -- Source: Changed benchmark CSV files
      benchmarks <- watchTree cwd (File.isBenchmarkCSV cwd)
      benchmarkedRepos <- repoOfFileEvent cwd activeReposB benchmarks

      -- Sink: produce the appropriate backlog and deploy
      let unite (t1, r1) (t2, r2) = (min t1 t2, Set.union r1 r2)
      let reposToFinish = Banana.unionWith unite fetchedRepos (second Set.singleton <$> benchmarkedRepos)
      finalizeLock <- liftIO Lock.new
      ios <- accumEM Map.empty (finalizeRepos finalizeLock paths deployment <$> activeReposB <@> reposToFinish)
      Banana.mapEventIO id ios
      Banana.reactimate ((>> return ()) <$> ios)

      -- Source: Backlog changes
      backlogs <- watchTree cwd (File.isBacklog cwd)
      backlogRepos <- repoOfFileEvent cwd activeReposB backlogs

      -- Sink: Backlog changes are propagated to the commit queue, which should
      --       know how to handle them. Also we exit when the queue is empty,
      --       provided we are in --one-shot mode.
      --   TODO: This exits prematurely when we start with a repo with an empty
      --         backlog regardless of other repos with non-empty backlogs.
      queueEmpty <- Banana.mapEventIO (updateCommitQueue commitQueue) (snd <$> backlogRepos)
      let doExit = when (mode == Once) (Event.set exit)
      Banana.reactimate ((`when` doExit) <$> queueEmpty)

  network <- Banana.compile networkDescription
  Banana.actuate network
  Event.set start
  Event.wait exit
