module Worker
  ( WorkItem (..)
  , work
  ) where


import           Control.Exception (mask, onException)
import           Control.Monad     (unless)
import           GitShell          (SHA)
import           Repo              (Repo)
import qualified Repo
import           System.Directory  (doesFileExist, removeFile)
import           System.FilePath   ((<.>), (</>))
import           System.IO         (IOMode (WriteMode), hClose, hPutStr,
                                    openFile)
import           System.Process    (readProcessWithExitCode)


data WorkItem
  = Benchmark FilePath Repo SHA
  | Regenerate FilePath Repo


writeFileDeleteOnException :: FilePath -> IO String -> IO ()
writeFileDeleteOnException path action =
  -- remove the file only when an exception happened, but close the handle
  -- at all costs.
  mask $ \restore -> do
    handle <- openFile path WriteMode
    restore (action >>= hPutStr handle) `onException` (hClose handle >> removeFile path)
    hClose handle


work :: WorkItem -> IO ()
work (Benchmark cloben repo commit) = do
  -- Handle a fresh commit by benchmarking
  clone <- Repo.cloneDir repo
  logs <- Repo.logsDir repo
  let
    logFile = logs </> commit <.> "log"
  exists <- doesFileExist logFile
  unless exists $
    -- A poor man's mutex. Delete the file only if there occurred an error while
    -- benchmarking (e.g. ctrl-c). Otherwise a re-run would not benchmark
    -- the touched commit.
    writeFileDeleteOnException logFile $ do
      putStrLn ("New commit " ++ Repo.uri repo ++ "@" ++ commit)
      (exitCode, stdout, stderr) <- readProcessWithExitCode cloben [clone, commit] ""
      return stdout
work (Regenerate gipeda repo) = do
  -- Regenerate the site by re-running gipeda in the projectDir.
  project <- Repo.projectDir repo
  (exitCode, stdout, stderr) <- readProcessWithExitCode gipeda ["-C", project, "-j"] ""
  putStrLn stdout
