module Acceptance
  ( tests
  ) where


import qualified Acceptance.Driver      as Driver
import qualified Acceptance.Files       as Files
import           Control.Monad          (filterM, unless, when, (<=<))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Managed  (managed, runManaged)
import           Data.List              (isInfixOf)
import           Data.Maybe             (isJust)
import           System.Directory       (doesFileExist, findExecutable,
                                         getDirectoryContents, makeAbsolute)
import           System.Exit            (ExitCode (..))
import           System.FilePath        (takeExtension, (</>))
import           System.IO.Temp         (withSystemTempDirectory)
import           Test.Tasty
import           Test.Tasty.HUnit       (Assertion, assertBool, assertEqual,
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
  ] where
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


daemon :: TestTree
daemon = testGroup "daemon mode" []


parallelization :: TestTree
parallelization = testGroup "parallelization" []


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
