module Worker
  ( benchmark
  ) where

{-| The 'meat' of the daemon. @work@ calls the @--benchmark@ script for
    new @Benchmark@ items resp. the @--gipeda@ script for new @Regenerate@
    items.
-}

import           Control.Logging as Logging
import qualified Data.Text       as Text
import           GitShell        (SHA)
import           Repo            (Repo)
import qualified Repo
import           System.Process  (cwd, proc, readCreateProcessWithExitCode)


executeIn :: Maybe FilePath -> FilePath -> [String] -> IO String
executeIn cwd executable args = do
  (exitCode, stdout, stderr) <-
    readCreateProcessWithExitCode (proc executable args) { cwd = cwd } ""
  return stdout


benchmark :: FilePath -> Repo -> SHA -> IO String
benchmark cloben repo commit = do
  -- Handle a fresh commit by benchmarking
  clone <- Repo.cloneDir repo
  Logging.log (Text.pack ("New commit " ++ Repo.uri repo ++ "@" ++ commit))
  executeIn Nothing cloben [Repo.uri repo, commit]
