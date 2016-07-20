{-| @benchmark@ contains the logic to be executed on slave nodes.
-}


module FeedGipeda.Slave
  ( benchmark
  ) where


import           Control.Logging     as Logging
import qualified Data.Text           as Text
import           FeedGipeda.GitShell (SHA)
import           FeedGipeda.Repo     (Repo)
import qualified FeedGipeda.Repo     as Repo
import           FeedGipeda.Types    (Timeout)
import           System.Exit         (ExitCode (..))
import           System.IO.Temp      (withSystemTempDirectory)
import           System.Process      (cwd, proc, readCreateProcessWithExitCode,
                                      shell, showCommandForUser)
import qualified System.Timeout


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


cloneRecursiveAndCheckout :: Repo -> SHA -> FilePath -> IO ()
cloneRecursiveAndCheckout repo commit cloneDir = do
  procReportingError repo commit Nothing "git" ["clone", "--quiet", Repo.uri repo, cloneDir]
  procReportingError repo commit (Just cloneDir) "git" ["reset", "--hard", commit]
  shellReportingError repo commit (Just cloneDir) "git submodule update --init --recursive --quiet"
  return ()


{-| Clones the given @repo@ at a specific @commit@ into a temporary directory.
    Then calls the @benchmarkScript@ within that directory and returns its output.

    Will be executed on slave nodes.
-}
benchmark :: String -> Repo -> SHA -> Timeout -> IO String
benchmark benchmarkScript repo commit timeout = do
  clone <- Repo.cloneDir repo
  Logging.log (Text.pack ("Benchmarking " ++ Repo.uri repo ++ "@" ++ commit))
  withSystemTempDirectory "feed-gipeda" $ \cloneDir -> do
    cloneRecursiveAndCheckout repo commit cloneDir
    res <- System.Timeout.timeout (ceiling (timeout * 10^6)) $
      shellReportingError repo commit (Just cloneDir) benchmarkScript
    case res of
      Just res -> return res
      Nothing -> do
        Logging.warn . Text.pack . unlines $
          [ "Benchmark script timed out (--timeout is " ++ show timeout ++ ")"
          , "At commit " ++ Repo.uri repo ++ "@" ++ commit
          ]
        return "build/timeout;1.0"
