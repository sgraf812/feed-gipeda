module Worker
  ( benchmark
  ) where

{-| @benchmark@ contains the logic to be executed on slave nodes.
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


procReportingError :: Repo -> SHA -> Maybe FilePath -> FilePath -> [String] -> IO String
procReportingError repo commit cwd cmd args = do
  (exitCode, stdout, stderr) <-
    readCreateProcessWithExitCode (proc cmd args) { cwd = cwd } ""
  reportError repo commit (showCommandForUser cmd args) exitCode stderr
  return stdout


shellReportingError :: Repo -> SHA -> Maybe FilePath -> FilePath -> IO String
shellReportingError repo commit cwd cmd = do
  (exitCode, stdout, stderr) <-
    readCreateProcessWithExitCode (shell cmd) { cwd = cwd } ""
  reportError repo commit cmd exitCode stderr
  return stdout


reportError :: Repo -> SHA -> String -> ExitCode -> String -> IO ()
reportError repo commit cmd code stderr =
  case code of
    ExitSuccess -> return ()
    ExitFailure c ->
      Logging.warn . Text.pack . unlines $
        [ "Benchmark script error"
        , "At commit " ++ Repo.uri repo ++ "@" ++ commit ++ ":"
        , cmd ++ ": exit code " ++ show c
        , stderr
        ]


cloneRecursiveAndCheckout :: Repo -> SHA -> FilePath -> IO String
cloneRecursiveAndCheckout repo commit cloneDir = do
  procReportingError repo commit Nothing "git" ["clone", "--quiet", Repo.uri repo, cloneDir]
  procReportingError repo commit (Just cloneDir) "git" ["reset", "--hard", commit]
  shellReportingError repo commit (Just cloneDir) "git submodule update --init --recursive --quiet"
  return "cloben"


{-| Clones the given @repo@ at a specific @commit@ into a temporary directory.
    Then calls the benchmark script within that directory and returns its output.
    The benchmark script is extracted from the gipeda.yaml file at the top-level
    of the repository's HEAD.

    Will be executed on slave nodes.
-}
benchmark :: Repo -> SHA -> IO String
benchmark repo commit = do
  clone <- Repo.cloneDir repo
  Logging.log (Text.pack ("Benchmarking " ++ Repo.uri repo ++ "@" ++ commit))
  withSystemTempDirectory "feed-gipeda" $ \cloneDir -> do
    benchmarkScript <- cloneRecursiveAndCheckout repo commit cloneDir
    shellReportingError repo commit (Just cloneDir) benchmarkScript
