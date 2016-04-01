{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

import Debug.Trace
import           Control.Concurrent       (forkIO, threadDelay)
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Exception        (bracket, bracketOnError, mask,
                                           onException)
import           Control.Monad            (forever, unless, when)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Default             (Default (def))
import           Data.Maybe               (fromJust)
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.String              (fromString)
import qualified Data.Time                as Time
import qualified Data.Yaml                as Yaml
import           GitShell                 (SHA)
import qualified GitShell
import           Repo                     (Repo, Repos)
import qualified Repo
import           System.Console.ArgParser (Descr (..), ParserSpec, andBy,
                                           optPos, parsedBy, withParseResult)
import           System.Directory
import           System.FilePath          (dropFileName, equalFilePath,
                                           replaceFileName, takeBaseName, (<.>),
                                           (</>))
import qualified System.FSNotify          as FS
import           System.IO                (IOMode (WriteMode), hClose, hPutStr,
                                           openFile)
import           System.Process           (readProcessWithExitCode)


parseRepos :: FilePath -> IO Repos
parseRepos f = do
  repos <- Yaml.decodeFile f
  -- TODO: error handling
  return (fromJust repos)


clonePath :: Repo -> IO FilePath
clonePath repo = do
  cwd <- getCurrentDirectory
  return (cwd </> Repo.uniqueName repo </> "repository")


logsPath :: Repo -> IO FilePath
logsPath repo = do
  cwd <- getCurrentDirectory
  return (cwd </> Repo.uniqueName repo </> "logs")


writeFileDeleteOnException :: FilePath -> IO String -> IO ()
writeFileDeleteOnException path action =
  -- remove the file only when an exception happened, but close the handle
  -- at all costs.
  mask $ \restore -> do
    handle <- openFile path WriteMode
    restore (action >>= hPutStr handle) `onException` (hClose handle >> putStrLn ("Removing file "++ path) >> removeFile path)
    hClose handle


benchmark :: String -> WorkItem -> IO ()
benchmark script (WorkItem repo commit) = do
  clone <- clonePath repo
  logs <- logsPath repo
  let
    logFile = logs </> commit <.> "log"
  exists <- doesFileExist logFile
  unless exists $
    -- A poor man's mutex. Delete the file only if there occurred an error while
    -- benchmarking (e.g. ctrl-c). Otherwise a re-run would not benchmark
    -- the touched commit.
    writeFileDeleteOnException logFile $ do
      putStrLn ("New commit " ++ Repo.uri repo ++ "@" ++ commit)
      (exitCode, stdout, stderr) <- readProcessWithExitCode script [clone, commit] ""
      return stdout


fetchRepos :: Chan WorkItem -> Repos -> IO ()
fetchRepos workItems repos =
  mapM_ fetchRepo (Repo.toList repos)
    where
      fetchRepo :: Repo -> IO ()
      fetchRepo repo = do
        unhandledCommits <- unhandledCommits repo
        mapM_ (writeChan workItems . WorkItem repo) unhandledCommits


unhandledCommits :: Repo -> IO (Set SHA)
unhandledCommits repo = do
  path <- clonePath repo
  hasClone <- doesDirectoryExist path
  if hasClone
    then do
      GitShell.fetch path
      allCommits <- GitShell.allCommits path
      logsPath <- logsPath repo
      createDirectoryIfMissing True logsPath
      alreadyHandledCommits <- getDirectoryContents logsPath
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


data CmdArgs
  = CmdArgs
  { benchmarkScript :: String
  }


cmdParser :: ParserSpec CmdArgs
cmdParser = CmdArgs
  `parsedBy` optPos "cloben" "benchmark" `Descr` "benchmark script which will be"
  ++ " supplied the repository to name and specific commit to benchmark"


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


periodicallyFetch :: Time.NominalDiffTime -> Chan WorkItem -> MVar Repos -> IO ()
periodicallyFetch dt workItems repos = forever $ do
  begin <- Time.getCurrentTime
  readMVar repos >>= fetchRepos workItems
  end <- Time.getCurrentTime
  let elapsed = Time.diffUTCTime end begin
  threadDelay (ceiling ((dt - elapsed) * 1000000))


data WorkItem
  = WorkItem Repo SHA


configFile :: IO FilePath
configFile =
  canonicalizePath "feed-gipeda.yaml"


touchIfPresent :: FilePath -> IO ()
touchIfPresent f = do
  exists <- doesFileExist f
  when exists $ do
      contents <- readFile f
      length contents `seq` writeFile f contents


main :: IO ()
main = withParseResult cmdParser $ \(CmdArgs script) -> do
  repos <- newMVar Repo.noRepos
  workItems <- newChan
  f <- configFile

  let
    benchmarkWorker :: IO ()
    benchmarkWorker =
      getChanContents workItems >>= mapM_ (benchmark script)

    cloneAddedRepos :: IO ()
    cloneAddedRepos =
      extractNewRepos repos f >>= fetchRepos workItems

    initialTouchAndFetchPeriodically :: IO ()
    initialTouchAndFetchPeriodically =
      touchIfPresent f >> periodicallyFetch 5 workItems repos

  -- The worker thread will perform the benchmarking and site generation
  -- by calling the appropriate scripts
  -- I'm unsure about whether this should be parallelized with multiple workers.
  -- Performance couldn't be compared among builds anymore.
  forkIO benchmarkWorker
  -- Each time the config file f changes, we @cloneAddedRepos@ (New repos with
  -- new SHAs).
  -- We also touch the config file initially (recognizing local clones and
  -- already benchmarked commits) and check all clones for updated remotes
  -- (old Repos, new SHAs).
  withWatchFile f cloneAddedRepos initialTouchAndFetchPeriodically
