{-# LANGUAGE OverloadedStrings #-}

module Acceptance
  ( tests
  ) where


import qualified Acceptance.Driver        as Driver
import qualified Acceptance.Files         as Files
import           Control.Concurrent       (ThreadId, forkIO, killThread,
                                           myThreadId, threadDelay)
import           Control.Concurrent.Async (link, race_, withAsync)
import           Control.Concurrent.MVar  (newEmptyMVar, putMVar, takeMVar)
import           Control.Exception        (AsyncException, SomeException,
                                           bracket, catch, handle, throwTo)
import           Control.Monad            (filterM, mfilter, unless, when,
                                           (<=<))
import           Control.Monad.IO.Class   (MonadIO (..))
import           Control.Monad.Managed    (Managed, managed, runManaged)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import           Data.Conduit             (Source, await, ($$), ($=), (=$))
import qualified Data.Conduit.Binary      as CB
import qualified Data.Conduit.List        as CLc
import           Data.Conduit.Process     (StreamingProcessHandle,
                                           waitForStreamingProcess)
import           Data.Functor
import           Data.List                (isInfixOf, isSuffixOf)
import           Data.Maybe               (fromJust, fromMaybe, isJust)
import           Data.Monoid              (Any (..))
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import           Network.URI              (parseURI)
import           System.Directory         (doesFileExist, findExecutable,
                                           getDirectoryContents, makeAbsolute)
import           System.Exit              (ExitCode (..))
import           System.FilePath          (takeDirectory, takeExtension, (</>))
import qualified System.FSNotify          as FS
import           System.IO                (hClose, hPutStrLn)
import           System.IO.Temp           (withSystemTempDirectory,
                                           withSystemTempFile)
import           Test.Tasty
import           Test.Tasty.HUnit         (Assertion, assertBool, assertEqual,
                                           assertFailure, testCase)


debugTests :: Bool
debugTests = True -- cringe


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
  [ testCase "well-formed file exits successfully" $ runManaged $ do
      (_, handle) <-
        Files.withWellFormedConfig >>= Driver.withCheckInTmpDir
      assertNormalExit handle
  ]


oneShot :: TestTree
oneShot = testGroup "one-shot mode"
  [ testCase "watching a single repo produces site/ files" $ runManaged $ do
      (path, handle) <-
        Files.withWellFormedConfig >>= Driver.withOneShotInTmpDir Nothing
      assertNormalExit handle
      assertSiteFolderComplete (path </> "feed-gipeda-test-11018374512395291872" </> "site")
  , testCase "watching a single repo with deployment" $ runManaged $ do
      deploymentDir <- managed (withSystemTempDirectory "feed-gipeda")
      (path, handle) <-
        Files.withWellFormedConfig >>= Driver.withOneShotInTmpDir (Just deploymentDir)
      assertNormalExit handle
      assertSiteFolderComplete (deploymentDir </> "sgraf812" </> "feed-gipeda-test")
  -- Test with multiple repos in config? There shouldn't be any new code paths.
  ]


daemon :: TestTree
daemon = testGroup "daemon mode"
  [ testCase "adding a repo to the config causes that repo to be added" $ runManaged $ do
      (config, handle) <- managed (withSystemTempFile "feed-gipeda.yaml" . curry)
      liftIO (hClose handle)
      deploymentDir <- managed (withSystemTempDirectory "feed-gipeda")
      (path, handle) <-
        Driver.withDaemonInTmpDir (Just deploymentDir) 3600 config
      assertReactsToChange handle deploymentDir
        (BS.writeFile config Files.wellFormedConfig)
  , testCase "adding commits to a repo under watch should trigger benchmarks" $ runManaged $ do
      liftIO (threadDelay 10000000) -- terminateProcess doesn't release the TCP ports, so we have to wait for the OS to catch up
      repo <- Files.withInitGitRepo
      (config, h) <- managed (withSystemTempFile "feed-gipeda.yaml" . curry)
      liftIO (hPutStrLn h "repositories:")
      liftIO (hPutStrLn h ("- file://" ++ repo))
      liftIO (hClose h)
      deploymentDir <- managed (withSystemTempDirectory "feed-gipeda")
      (path, handle) <-
        Driver.withDaemonInTmpDir (Just deploymentDir) 5 config
      assertReactsToChange handle deploymentDir
        (Files.makeCloneOf repo (fromJust $ parseURI "https://github.com/sgraf812/feed-gipeda-test"))
  ]


parallelization :: TestTree
parallelization = testGroup "parallelization"
  [ testCase "does not benchmark in master mode" $ runManaged $ do
      (path, handle) <-
        Files.withWellFormedConfig >>= Driver.withMasterInTmpDir 12345
      withAssertNotExit handle
      assertCsvFilesDontChangeWithin 100 path
  , testCase "can distribute work on slave nodes" $ runManaged $ do
      (path, handle) <-
        Files.withWellFormedConfig >>= Driver.withMasterInTmpDir 12345
      withAssertNotExit handle
      spawnSlave 12346
      spawnSlave 12347
      spawnSlave 12348
      spawnSlave 12349
      assertCsvFilesChangeWithin 100 path
  ]
  where
    spawnSlave port = do
      h <- Driver.withSlave port
      withAssertNotExit h


assertReactsToChange
  :: MonadIO io
  => StreamingProcessHandle
  -> String
  -> IO ()
  -> io ()
assertReactsToChange handle deploymentDir changeAction = liftIO $ runManaged $ do
  withAssertNotExit handle
  liftIO (threadDelay 5000000) -- ouch
  liftIO changeAction
  assertCsvFilesChangeWithin 100 deploymentDir


catchAsyncException :: IO () -> IO ()
catchAsyncException =
  handle handler
    where
      handler :: AsyncException -> IO ()
      handler e = return ()


withAssertNotExit :: StreamingProcessHandle -> Managed ()
withAssertNotExit handle = do
  asy <- managed $ withAsync $ catchAsyncException $ do
    waitForStreamingProcess handle
    threadDelay 50 -- So that failures due to stdout/stderr have precedence
    threadDelay 50
    assertFailure "must not exit"
  liftIO $ link asy


withAssertNoOutput :: Source IO ByteString -> String -> Managed ()
withAssertNoOutput content name = do
  asy <- managed $ withAsync $ catchAsyncException $ content $= CB.lines $$ do
      line <- await
      case mfilter (not . BS.null) line of
        Nothing -> return ()
        Just sth ->
          liftIO $ assertFailure $
            "should not write any output to " ++ name ++ ". Got: " ++ Text.unpack (Text.decodeUtf8 sth)
  liftIO $ link asy


assertNormalExit
  :: StreamingProcessHandle
  -> Managed ()
assertNormalExit handle = do
  liftIO $ do
    exitCode <- waitForStreamingProcess handle
    threadDelay 50 -- So that failures due to stdout/stderr have precedence
    threadDelay 50
    assertEqual "should exit successfully" ExitSuccess exitCode


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
  jsons <- liftIO $ filesInDirWithExt ".json" (site </> "out" </> "graphs" </> "stub")
  assertNotEqual "should produce some json data through gipeda" [] (filter ((== ".json") . takeExtension) jsons)


filesInDirWithExt :: String -> FilePath -> IO [FilePath]
filesInDirWithExt ext dir =
  map (dir </>) . filter ((== ext) . takeExtension) <$> getDirectoryContents dir


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
