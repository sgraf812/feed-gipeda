module Acceptance
  ( tests
  ) where


import qualified Acceptance.Driver       as Driver
import qualified Acceptance.Files        as Files
import           Control.Concurrent      (ThreadId, forkIO, killThread,
                                          myThreadId, threadDelay)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import           Control.Exception       (bracket, catch, throwTo)
import           Control.Monad           (filterM, unless, when, (<=<))
import           Control.Monad.IO.Class  (MonadIO (..))
import           Control.Monad.Managed   (Managed, managed, runManaged)
import qualified Data.ByteString         as BS
import           Data.List               (isInfixOf, isSuffixOf)
import           Data.Maybe              (fromJust, isJust)
import           Network.URI             (parseURI)
import           System.Directory        (doesFileExist, findExecutable,
                                          getDirectoryContents, makeAbsolute)
import           System.Exit             (ExitCode (..))
import           System.FilePath         (takeDirectory, takeExtension, (</>))
import qualified System.FSNotify         as FS
import           System.IO               (hClose, hPutStrLn)
import           System.IO.Temp          (withSystemTempDirectory,
                                          withSystemTempFile)
import           Test.HUnit.Lang         (HUnitFailure)
import           Test.Tasty
import           Test.Tasty.HUnit        (Assertion, assertBool, assertEqual,
                                          assertFailure, testCase)


tests ::  TestTree
tests = testGroup "Acceptance tests"
  [ testCase "has feed-gipeda in $PATH" $ do
      path <- findExecutable "feed-gipeda"
      assertBool "feed-gipeda should exist" (isJust path)
  , check
  , oneShot
  , daemon
  , parallelization
  ]


check :: TestTree
check = testGroup "check"
  [ testCase "malformed file exits with error" $ runManaged $ do
      (_, exitCode, _, stderr) <-
        Files.withMalformedConfig >>= Driver.withCheckInTmpDir
      liftIO $ assertNotEqual "exited successfully" ExitSuccess exitCode
      liftIO $ assertBool "no YAML error" ("YAML" `isInfixOf` stderr)
  , testCase "well-formed file exits successfully" $ runManaged $ do
      configFile <- Files.withWellFormedConfig
      (_, exitCode, stdout, stderr) <-
        Files.withWellFormedConfig >>= Driver.withCheckInTmpDir
      assertExitSuccessfully exitCode
      assertNoErrors stderr
      assertNoOutput stdout
  ]


oneShot :: TestTree
oneShot = testGroup "one-shot mode"
  [ testCase "watching a single repo produces site/ files" $ runManaged $ do
      (path, exitCode, stdout, stderr) <-
        Files.withWellFormedConfig >>= Driver.withOneShotInTmpDir Nothing
      assertNormalExit exitCode stderr
      assertSiteFolderComplete (path </> "benchmark-test-6085726404018277061" </> "site")
  , testCase "watching a single repo with deployment" $ runManaged $ do
      deploymentDir <- managed (withSystemTempDirectory "feed-gipeda")
      (path, exitCode, stdout, stderr) <-
        Files.withWellFormedConfig >>= Driver.withOneShotInTmpDir (Just deploymentDir)
      assertNormalExit exitCode stderr
      assertSiteFolderComplete (deploymentDir </> "sgraf812" </> "benchmark-test")
  -- Test with multiple repos in config? There shouldn't be any new code paths.
  ]


daemon :: TestTree
daemon = testGroup "daemon mode"
  [ testCase "adding a repo to the config causes that repo to be added" $ runManaged $ do
      (config, handle) <- managed (withSystemTempFile "feed-gipeda.yaml" . curry)
      liftIO (hClose handle)
      deploymentDir <- managed (withSystemTempDirectory "feed-gipeda")
      (path, daemon) <- Driver.withDaemonInTmpDir (Just deploymentDir) Nothing config
      spawnAssertNotExit daemon
      liftIO (threadDelay 5000000) -- ouch
      liftIO (BS.writeFile config Files.wellFormedConfig)
      assertCsvFilesChangeWithin 300 deploymentDir
  , testCase "adding commits to a repo under watch should trigger benchmarks" $ runManaged $ do
      repo <- Files.withInitGitRepo
      (config, handle) <- managed (withSystemTempFile "feed-gipeda.yaml" . curry)
      liftIO (hPutStrLn handle "repositories:")
      liftIO (hPutStrLn handle ("- file://" ++ repo))
      liftIO (hClose handle)
      deploymentDir <- managed (withSystemTempDirectory "feed-gipeda")
      (path, daemon) <- Driver.withDaemonInTmpDir (Just deploymentDir) (Just 5) config
      spawnAssertNotExit daemon
      liftIO (threadDelay 5000000)
      liftIO $ Files.makeCloneOf repo (fromJust $ parseURI "https://github.com/sgraf812/benchmark-test")
      assertCsvFilesChangeWithin 300 deploymentDir
  ]


parallelization :: TestTree
parallelization = testGroup "parallelization"
  [ testCase "does not benchmark in master mode" $ runManaged $ do
      (path, master) <-
        Files.withWellFormedConfig >>= Driver.withMasterInTmpDir 12345
      spawnAssertNotExit master
      assertCsvFilesDontChangeWithin 100 path
  , testCase "can distribute work on slave nodes" $ runManaged $ do
      (path, master) <-
        Files.withWellFormedConfig >>= Driver.withMasterInTmpDir 12345
      spawnAssertNotExit master
      spawnSlave 12346
      spawnSlave 12347
      spawnSlave 12348
      spawnSlave 12349
      assertCsvFilesChangeWithin 300 path
  ]
  where
    spawnSlave port =
      spawnAssertNotExit (Driver.slave port)



spawnAssertNotExit :: IO (ExitCode, String, String) -> Managed ThreadId
spawnAssertNotExit proc = do
  pid <- liftIO myThreadId
  let forkAndKill action = managed $ bracket (forkIO action) killThread
  forkAndKill $ do
    (_, stdout, stderr) <- proc
    catch -- That catch doesn't seem to work, don't know why.
      (assertFailure "daemon should not exit")
      (\e -> throwTo pid (e :: HUnitFailure))


assertCsvFilesWithin :: MonadIO io => (Bool -> Bool) -> Int -> FilePath -> io ()
assertCsvFilesWithin isOk seconds treeRoot = liftIO $ runManaged $ do
  mgr <- managed FS.withManager
  liftIO $ do
    var <- newEmptyMVar
    let matches fp = takeExtension fp == ".csv" && "results" `isSuffixOf` takeDirectory fp
    FS.watchTree mgr treeRoot (matches . FS.eventPath) $ \path ->
      putMVar var True
    forkIO $ do
      threadDelay (seconds * 1000000)
      putMVar var False
    result <- takeMVar var
    unless (isOk result) (assertFailure ((if result then "" else "No ") ++ "CSV change within timeout"))


assertCsvFilesDontChangeWithin :: MonadIO io => Int -> FilePath -> io ()
assertCsvFilesDontChangeWithin =
  assertCsvFilesWithin not


assertCsvFilesChangeWithin :: MonadIO io => Int -> FilePath -> io ()
assertCsvFilesChangeWithin =
  assertCsvFilesWithin id


assertSiteFolderComplete :: MonadIO io => FilePath -> io ()
assertSiteFolderComplete site = do
  -- Benchmark results
  csvs <- liftIO $ filesInDirWithExt ".csv" (site </> "out" </> "results")
  assertNotEqual "should produce some result files" [] csvs
  nonEmptyCsvs <- liftIO $ filterM (fmap (not . null) . readFile) csvs
  assertNotEqual "should produce some non-empty result files" [] nonEmptyCsvs

  -- Gipeda files
  jsons <- liftIO $ filesInDirWithExt ".json" (site </> "out" </> "graphs" </> "benchmarks" </> "fib")
  assertNotEqual "should produce some json data through gipeda" [] (filter ((== ".json") . takeExtension) jsons)


filesInDirWithExt :: String -> FilePath -> IO [FilePath]
filesInDirWithExt ext dir =
  map (dir </>) . filter ((== ext) . takeExtension) <$> getDirectoryContents dir


assertNormalExit :: MonadIO io => ExitCode -> String -> io ()
assertNormalExit exitCode stderr = do
  assertExitSuccessfully exitCode
  assertNoErrors stderr
  -- assertNoOutput stdout -- TODO: Uncomment when the backlog.txt stuff gets into gipeda


assertExitSuccessfully :: MonadIO io => ExitCode -> io ()
assertExitSuccessfully exitCode =
  liftIO $ assertEqual "should exit successfully" ExitSuccess exitCode


assertNoErrors :: MonadIO io => String -> io ()
assertNoErrors stderr =
  liftIO $ assertEqual "should not write any errors" "" stderr


assertNoOutput :: MonadIO io => String -> io ()
assertNoOutput stdout =
  liftIO $ assertEqual "should not write any output" "" stdout


assertNotEqual
  :: (Eq a, Show a, MonadIO io)
  => String -- ^ The message prefix
  -> a      -- ^ The expected value
  -> a      -- ^ The actual value
  -> io ()
assertNotEqual preface expected actual =
  unless (actual /= expected) (liftIO (assertFailure msg))
    where
      msg =
        (if null preface then "" else preface ++ "\n")
        ++ "expected not to get: " ++ show expected ++ "\n but got: " ++ show actual
