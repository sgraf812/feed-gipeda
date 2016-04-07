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
import           Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import           Control.Monad           (forever, unless, when)
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


fetchRepos :: NewCommitsAction -> Set Repo -> IO ()
fetchRepos onNewCommits repos =
  mapM_ fetchRepo (Set.toList repos)
    where
      fetchRepo :: Repo -> IO ()
      fetchRepo repo =
        unhandledCommits repo >>= onNewCommits repo


{-| Unhandled commits of a repo are those where there is no corresponding
    file in the results directory needed by gipeda of the repo.
-}
unhandledCommits :: Repo -> IO (Set SHA)
unhandledCommits repo = do
  path <- Repo.cloneDir repo
  hasClone <- GitShell.isRepositoryRoot path
  if hasClone
    then do
      GitShell.fetch path
      allCommits <- GitShell.allCommits path
      resultsDir <- Repo.resultsDir repo
      createDirectoryIfMissing True resultsDir
      alreadyHandledCommits <- getDirectoryContents resultsDir
      (return . Set.difference allCommits . Set.fromList . map takeBaseName) alreadyHandledCommits
    else do
      createDirectoryIfMissing True path
      GitShell.cloneBare repo path
      GitShell.allCommits path


extractNewRepos :: MVar (Set Repo) -> Set Repo -> IO (Set Repo)
extractNewRepos reposVar newRepos = do
  putStrLn "Extracting new repositories from the config file..."
  modifyMVar reposVar $ \oldRepos ->
    return (newRepos, Set.difference newRepos oldRepos)


periodicallyRefreshRepos :: NominalDiffTime -> NewCommitsAction -> MVar (Set Repo) -> IO ()
periodicallyRefreshRepos dt onNewCommits repos = forever $ do
  begin <- Time.getCurrentTime
  readMVar repos >>= fetchRepos onNewCommits
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
  repos <- newMVar Set.empty

  let
    cloneAddedRepos :: Config -> IO ()
    cloneAddedRepos config =
      extractNewRepos repos (Config.repos config) >>= fetchRepos onNewCommits

    initialTouchAndFetchPeriodically :: IO ()
    initialTouchAndFetchPeriodically =
      touchIfPresent configFile >> periodicallyRefreshRepos dt onNewCommits repos

  Config.withWatchFile configFile cloneAddedRepos initialTouchAndFetchPeriodically
