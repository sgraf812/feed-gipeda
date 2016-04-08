{-| Handles all the config file watching and repository updating.
    Executes an IO action when a repository was (re-)fetched, also supplying
    commits, for which there was no results file found in site/out/results.

    Repositories are re-fetched at a fixed rate. Existing clones are detected
    and reused, so that restarting the daemon will not do unnecessary work.
-}

module RepoWatcher
  ( watchConfiguredRepos
  ) where


import           Config                  (Config)
import qualified Config
import           Control.Concurrent      (threadDelay)
import           Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_,
                                          newMVar, readMVar)
import           Control.Monad           (forever, unless, when)
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Maybe              (fromJust)
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           Data.Time               (NominalDiffTime)
import qualified Data.Time               as Time
import           GitShell                (SHA)
import qualified GitShell
import           Repo                    (Repo)
import qualified Repo
import           System.Directory        (createDirectoryIfMissing,
                                          doesDirectoryExist, doesFileExist,
                                          getDirectoryContents)
import           System.FilePath         (takeBaseName)


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
        (unfinished, finished) <- commitDiff repo
        -- Don't fire for commits already in progress
        onNewCommits repo (Set.difference unfinished inProgress)
        -- Set the backlog to all unfinished commits
        modifyMVar_ backlog (return . Map.insert repo unfinished)


{-| Unfinished commits of a repo are those where there is no corresponding
    file in the results directory needed by gipeda of the repo.
-}
commitDiff :: Repo -> IO (Set SHA, Set SHA)
commitDiff repo = do
  path <- Repo.cloneDir repo
  hasClone <- GitShell.isRepositoryRoot path
  if hasClone
    then do
      GitShell.fetch path
      allCommits <- GitShell.allCommits path
      resultsDir <- Repo.resultsDir repo
      createDirectoryIfMissing True resultsDir
      alreadyHandledCommits <- Set.fromList . map takeBaseName <$> getDirectoryContents resultsDir
      return
        ( Set.difference allCommits alreadyHandledCommits    -- unfinished
        , Set.intersection allCommits alreadyHandledCommits) -- finished
    else do
      createDirectoryIfMissing True path
      GitShell.cloneBare repo path
      unfinished <- GitShell.allCommits path
      return (unfinished, Set.empty)


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


watchConfiguredRepos :: FilePath -> NominalDiffTime -> NewCommitsAction -> IO ()
watchConfiguredRepos configFile dt onNewCommits = do
  repos <- newMVar Map.empty

  let
    cloneAddedRepos :: Config -> IO ()
    cloneAddedRepos config =
      extractAddedRepos repos (Config.repos config) >> fetchRepos onNewCommits repos

    initialTouchAndFetchPeriodically :: IO ()
    initialTouchAndFetchPeriodically =
      touchIfPresent configFile >> periodicallyRefreshRepos dt onNewCommits repos

  Config.withWatchFile configFile cloneAddedRepos initialTouchAndFetchPeriodically
