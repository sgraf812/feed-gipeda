{-| @benchmark@ contains the logic to be executed on slave nodes.
-}


module FeedGipeda.Slave
  ( benchmark
  ) where


import           Control.Applicative
import           Control.Concurrent.Async (Concurrently (..))
import           Control.Exception        (bracket)
import           Control.Logging          as Logging
import           Data.Conduit             (($$))
import qualified Data.Conduit.List        as CL
import           Data.Conduit.Process     (ClosedStream (..),
                                           CreateProcess (..),
                                           interruptProcessGroupOf, proc,
                                           readCreateProcessWithExitCode, shell,
                                           showCommandForUser, streamingProcess,
                                           streamingProcessHandleRaw,
                                           waitForStreamingProcess)
import           Data.Monoid
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import           FeedGipeda.GitShell      (SHA)
import           FeedGipeda.Prelude
import           FeedGipeda.Repo          (Repo)
import qualified FeedGipeda.Repo          as Repo
import           FeedGipeda.Types         (Timeout)
import           System.Exit              (ExitCode (..))
import           System.IO.Temp           (withSystemTempDirectory)
import qualified System.Timeout


-- We have to roll our own, because the provided functions use @terminateProcess@
-- instead of @interruptProcessGroupOf@, so that in case of shelling out we only
-- kill the shell process but not its children.
readCreateProcessGroupWithExitCode :: CreateProcess -> IO (ExitCode, String, String)
readCreateProcessGroupWithExitCode cp =
  bracket
    (streamingProcess cp { create_group = True })
    cleanup
    captureStreams
  where
    cleanup (_, _, _, sph) =
      interruptProcessGroupOf (streamingProcessHandleRaw sph)

    captureStreams (ClosedStream, out, err, sph) =
      runConcurrently $ (,,)
        <$> Concurrently (waitForStreamingProcess sph)
        <*> Concurrently (out $$ stringSink)
        <*> Concurrently (err $$ stringSink)

    stringSink =
      Text.unpack . Text.decodeUtf8 <$> CL.fold (<>) mempty


procReportingError :: Repo -> SHA -> Maybe FilePath -> FilePath -> [String] -> IO String
procReportingError repo commit cwd cmd args = do
  (exitCode, stdout, stderr) <-
    readCreateProcessWithExitCode (proc cmd args) { cwd = cwd } ""
  reportError repo commit (showCommandForUser cmd args) exitCode stderr
  return stdout


shellReportingError :: Repo -> SHA -> Maybe FilePath -> FilePath -> IO String
shellReportingError repo commit cwd cmd = do
  (exitCode, stdout, stderr) <-
    readCreateProcessGroupWithExitCode (shell cmd) { cwd = cwd }
  reportError repo commit cmd exitCode stderr
  return stdout


reportError :: Repo -> SHA -> String -> ExitCode -> String -> IO ()
reportError repo commit cmd code stderr =
  case code of
    ExitSuccess -> return ()
    ExitFailure c ->
      logWarn . unlines $
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
  logInfo ("Benchmarking " ++ Repo.uri repo ++ "@" ++ commit)
  withSystemTempDirectory "feed-gipeda" $ \cloneDir -> do
    cloneRecursiveAndCheckout repo commit cloneDir
    res <- System.Timeout.timeout (ceiling (timeout * 10^6)) $
      shellReportingError repo commit (Just cloneDir) benchmarkScript
    case res of
      Just res -> do
        logInfo "Finished. Output:"
        mapM_ logInfo (lines res)
        return res
      Nothing -> do
        logWarn . unlines $
          [ "Benchmark script timed out (--timeout is " ++ show timeout ++ ")"
          , "At commit " ++ Repo.uri repo ++ "@" ++ commit
          ]
        return "build/timeout;1.0"
