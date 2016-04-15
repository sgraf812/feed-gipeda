{-| Handles all the config file watching and repository updating.
    Executes an IO action when a repository was (re-)fetched, also supplying
    commits, for which there was no results file found in site/out/results.

    Repositories are re-fetched at a fixed rate. Existing clones are detected
    and reused, so that restarting the daemon will not do unnecessary work.
-}

module RepoWatcher
  ( watchConfiguredRepos
  , commitDiff
  ) where


import           Config                      (Config)
import qualified Config
import           Control.Concurrent          (threadDelay)
import           Control.Concurrent.Chan     (Chan, newChan, readChan,
                                              writeChan)
import           Control.Concurrent.MVar     (MVar, modifyMVar, modifyMVar_,
                                              newEmptyMVar, newMVar, putMVar,
                                              readMVar)
import           Control.Distributed.Process (Process, liftIO, say, spawnLocal)
import           Control.Monad               (forM_, forever, unless, when)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Maybe                  (isNothing)
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Time                   (NominalDiffTime)
import qualified Data.Time                   as Time
import           GitShell                    (SHA)
import qualified GitShell
import           Repo                        (Repo)
import qualified Repo
import           System.Directory            (createDirectoryIfMissing,
                                              doesDirectoryExist, doesFileExist,
                                              getDirectoryContents)
import           System.FilePath             (takeBaseName)


type NewCommitAction
  = Repo -> SHA -> Process ()


type NewCommitsAction
  = Repo -> Set SHA -> IO ()


type Backlog
  = Map Repo (Set SHA)


fetchRepos :: NewCommitsAction -> MVar Backlog -> IO ()
fetchRepos onNewCommits backlog = do
  currentBacklog <- readMVar backlog
  mapM_ fetchRepo (Map.assocs currentBacklog)
    where
      fetchRepo :: (Repo, Set SHA) -> IO ()
      fetchRepo (repo, inProgress) = do
        path <- Repo.cloneDir repo
        hasClone <- GitShell.isRepositoryRoot path
        if hasClone
          then GitShell.fetch path
          else do
            createDirectoryIfMissing True path
            GitShell.cloneBare repo path
        (unfinished, finished) <- commitDiff repo
        -- Set the backlog to all unfinished commits
        modifyMVar_ backlog (return . Map.insert repo unfinished)
        -- Don't fire for commits already in progress
        onNewCommits repo (Set.difference unfinished inProgress)


{-| Unfinished commits of a repo are those where there is no corresponding
    file in the results directory needed by gipeda of the repo.
-}
commitDiff :: Repo -> IO (Set SHA, Set SHA)
commitDiff repo = do
  path <- Repo.cloneDir repo
  hasClone <- GitShell.isRepositoryRoot path
  if not hasClone
    then return (Set.empty, Set.empty)
    else do
      allCommits <- GitShell.allCommits path
      resultsDir <- Repo.resultsDir repo
      createDirectoryIfMissing True resultsDir
      alreadyHandledCommits <- Set.fromList . map takeBaseName <$> getDirectoryContents resultsDir
      return
        ( Set.difference allCommits alreadyHandledCommits    -- unfinished
        , Set.intersection allCommits alreadyHandledCommits  -- finished
        )


extractAddedRepos :: MVar Backlog -> Set Repo -> IO (Set Repo)
extractAddedRepos backlog newRepos = do
  putStrLn "Extracting new repositories from the config file..."
  modifyMVar backlog $ \oldBacklog -> do
    let
      addedKeys :: Set Repo
      addedKeys =
        Set.difference newRepos (Map.keysSet oldBacklog)

      newBacklog :: Backlog
      newBacklog =
        Map.fromSet (const Set.empty) newRepos -- no commit is in flight for new repos

    -- we have to take the keys from newRepos, but use the values
    -- from oldBacklog where possible. So the following should do:
    return ((oldBacklog `Map.union` newBacklog) `Map.intersection` newBacklog , addedKeys)


periodicallyRefreshRepos :: NominalDiffTime -> NewCommitsAction -> MVar Backlog -> IO ()
periodicallyRefreshRepos dt onNewCommits backlog = forever $ do
  begin <- Time.getCurrentTime
  fetchRepos onNewCommits backlog
  end <- Time.getCurrentTime
  let elapsed = Time.diffUTCTime end begin
  threadDelay (ceiling ((dt - elapsed) * 1000000))


touchIfPresent :: FilePath -> IO ()
touchIfPresent f = do
  exists <- doesFileExist f
  when exists $ do
    contents <- readFile f
    length contents `seq` writeFile f contents


watchConfiguredRepos :: FilePath -> Maybe NominalDiffTime -> NewCommitAction -> Process ()
watchConfiguredRepos configFile dt onNewCommit = do
  backlog <- liftIO (newMVar Map.empty)
  allFinishedEvent <- liftIO newEmptyMVar
  unfinishedRepos <- liftIO (newMVar Set.empty)
  deferredEvents <- liftIO newChan

  let
    oneShot :: Bool
    oneShot =
      isNothing dt

    cloneAddedRepos :: Config -> IO ()
    cloneAddedRepos config = do
      extractAddedRepos backlog (Config.repos config)
      fetchRepos onNewCommits backlog
      when oneShot $ do
        bl <- readMVar backlog
        modifyMVar_ unfinishedRepos $ \_ ->
          return (Map.keysSet (Map.filter (not . Set.null) bl))

    onNewCommits :: NewCommitsAction
    onNewCommits repo commits =
      writeChan deferredEvents (repo, commits)

    waitAction :: IO ()
    waitAction =
      case dt of
        Just dt' ->
          Config.withWatchFile
            configFile
            cloneAddedRepos
            (periodicallyRefreshRepos dt' onNewCommits backlog)
        Nothing ->
          readMVar allFinishedEvent -- block until all commits are handled

    exitCheck :: IO ()
    exitCheck = do
      canExit <- Set.null <$> readMVar unfinishedRepos
      when canExit (putMVar allFinishedEvent ())

  -- We can't call onNewCommits directly because the file watching APIs have
  -- @IO@ in negative position. So we translate by using a channel and a
  -- separate top-level process.
  -- Also this way we can detect when all commits are handled and we can quit
  -- (in the case of --one-shot)
  spawnLocal $ forever $ do
    (repo, commits) <- liftIO (readChan deferredEvents)
    forM_ commits $ \commit -> spawnLocal $ do
      onNewCommit repo commit
      liftIO $ when (isNothing dt) $ do
        -- In this case we have to detect when all commits are handled
        allCommitsHandled <- Set.null . fst <$> liftIO (commitDiff repo)
        when allCommitsHandled $ do
          modifyMVar_ unfinishedRepos (return . Set.delete repo)
          exitCheck

  liftIO (Config.decodeFileAndNotify cloneAddedRepos configFile)
  liftIO exitCheck -- In case there were no new commits
  liftIO waitAction
