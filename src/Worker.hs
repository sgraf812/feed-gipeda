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
import           System.Exit     (ExitCode (..))
import           System.IO.Temp  (withSystemTempDirectory)
import           System.Process  (cwd, proc, readCreateProcessWithExitCode,
                                  shell, showCommandForUser)


procReportingError :: Maybe FilePath -> FilePath -> [String] -> IO String
procReportingError cwd cmd args = do
  (exitCode, stdout, stderr) <-
    readCreateProcessWithExitCode (proc cmd args) { cwd = cwd } ""
  reportError (showCommandForUser cmd args) exitCode stderr
  return stdout


shellReportingError :: Maybe FilePath -> FilePath -> IO String
shellReportingError cwd cmd = do
  (exitCode, stdout, stderr) <-
    readCreateProcessWithExitCode (shell cmd) { cwd = cwd } ""
  reportError cmd exitCode stderr
  return stdout


reportError :: String -> ExitCode -> String -> IO ()
reportError cmd code stderr =
  case code of
    ExitSuccess -> return ()
    ExitFailure c -> do
      Logging.warn (Text.pack (cmd ++ ": exit code " ++ show c))
      Logging.warn (Text.pack stderr)


cloneRecursiveAndCheckout :: Repo -> SHA -> FilePath -> IO ()
cloneRecursiveAndCheckout repo commit cloneDir = do
  procReportingError Nothing "git" ["clone", "--quiet", Repo.uri repo, cloneDir]
  procReportingError (Just cloneDir) "git" ["reset", "--hard", commit]
  shellReportingError (Just cloneDir) "git submodule update --init --recursive --quiet"
  return ()


benchmark :: FilePath -> Repo -> SHA -> IO String
benchmark benchmarkScript repo commit = do
  clone <- Repo.cloneDir repo
  Logging.log (Text.pack ("Benchmarking " ++ Repo.uri repo ++ "@" ++ commit))
  withSystemTempDirectory "feed-gipeda" $ \cloneDir -> do
    cloneRecursiveAndCheckout repo commit cloneDir
    shellReportingError (Just cloneDir) benchmarkScript
