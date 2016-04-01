{-# LANGUAGE BangPatterns #-}

module RepoWatcher
  ( watchConfiguredRepos
  ) where


import           Control.Concurrent      (threadDelay)
import           Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import           Control.Monad           (forever, unless, when)
import           Data.Maybe              (fromJust)
import           Data.Set                (Set)
import qualified Data.Set                as Set
import qualified Data.Time               as Time
import qualified Data.Yaml               as Yaml
import           GitShell                (SHA)
import qualified GitShell
import           Repo                    (Repo, Repos)
import qualified Repo
import           System.Directory        (createDirectoryIfMissing,
                                          doesDirectoryExist, doesFileExist,
                                          getDirectoryContents)
import           System.FilePath         (dropFileName, equalFilePath,
                                          takeBaseName)
import qualified System.FSNotify         as FS


type NewCommitsAction
  = Repo -> Set SHA -> IO ()


parseRepos :: FilePath -> IO Repos
parseRepos f = do
  repos <- Yaml.decodeFile f
  -- TODO: error handling
  return (fromJust repos)


fetchRepos :: NewCommitsAction -> Repos -> IO ()
fetchRepos onNewCommits repos =
  mapM_ fetchRepo (Repo.toList repos)
    where
      fetchRepo :: Repo -> IO ()
      fetchRepo repo =
        unhandledCommits repo >>= onNewCommits repo
        --mapM_ (onNewCommit repo) unhandledCommits
        --unless (Set.empty unhandledCommits) (onUpdatedRepo repo)


unhandledCommits :: Repo -> IO (Set SHA)
unhandledCommits repo = do
  path <- Repo.cloneDir repo
  hasClone <- doesDirectoryExist path
  if hasClone
    then do
      GitShell.fetch path
      allCommits <- GitShell.allCommits path
      logsDir <- Repo.logsDir repo
      createDirectoryIfMissing True logsDir
      alreadyHandledCommits <- getDirectoryContents logsDir
      (return . Set.difference allCommits . Set.fromList . map takeBaseName) alreadyHandledCommits
    else do
      createDirectoryIfMissing True path
      GitShell.cloneBare repo path
      GitShell.allCommits path


extractNewRepos :: MVar Repos -> FilePath -> IO Repos
extractNewRepos reposVar file = do
  putStrLn "Extracting new repositories from the config file..."
  !newList <- parseRepos file
  modifyMVar reposVar $ \oldList ->
    return (newList, Repo.difference newList oldList)


withWatchFile :: FilePath -> IO () -> IO a -> IO a
withWatchFile file modifyAction inner = do
  let
    filterEvents evt =
      case evt of
        FS.Removed _ _ -> False
        _ -> equalFilePath file (FS.eventPath evt)
  FS.withManager $ \mgr -> do
    FS.watchDir mgr (dropFileName file) filterEvents (const modifyAction)
    inner


periodicallyRefreshRepos :: Time.NominalDiffTime -> NewCommitsAction -> MVar Repos -> IO ()
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


watchConfiguredRepos :: FilePath -> NewCommitsAction -> IO ()
watchConfiguredRepos config onNewCommits = do
  repos <- newMVar Repo.noRepos

  let
    cloneAddedRepos :: IO ()
    cloneAddedRepos =
      extractNewRepos repos config >>= fetchRepos onNewCommits

    initialTouchAndFetchPeriodically :: IO ()
    initialTouchAndFetchPeriodically =
      touchIfPresent config >> periodicallyRefreshRepos 5 onNewCommits repos

  withWatchFile config cloneAddedRepos initialTouchAndFetchPeriodically
