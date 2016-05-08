module Acceptance
  ( tests
  ) where


import qualified Acceptance.Driver     as Driver
import qualified Acceptance.Files      as Files
import           Control.Monad         (unless, when)
import           Control.Monad.Managed (liftIO, runManaged)
import           Data.List             (isInfixOf)
import           System.Exit           (ExitCode (..))
import           Test.Tasty
import           Test.Tasty.HUnit      (Assertion, assertBool, assertEqual,
                                        assertFailure, testCase)


tests ::  TestTree
tests = testGroup "Acceptance tests"
  [ check
  , oneShot
  , daemon
  , parallelization
  ]


check :: TestTree
check = testGroup "check"
  [ testCase "malformed file exits with error" $ runManaged $ do
      configFile <- Files.withMalformedConfig
      (_, exitCode, _, stderr) <-
        Driver.withExecuteInTmpDir (Driver.defaultConfig configFile) { Driver.check = True }
      liftIO $ assertNotEqual "exited successfully" ExitSuccess exitCode
      liftIO $ assertBool "wrong error message" ("YAML" `isInfixOf` stderr)
  , testCase "well-formed file exits successfully" $ runManaged $ do
      configFile <- Files.withWellFormedConfig
      (_, exitCode, _, stderr) <-
        Driver.withExecuteInTmpDir (Driver.defaultConfig configFile) { Driver.check = True }
      liftIO $ assertEqual "should exit successfully" ExitSuccess exitCode
      liftIO $ assertEqual "should not write any errors" "" stderr
  ]


oneShot :: TestTree
oneShot = testGroup "one-shot mode" []


daemon :: TestTree
daemon = testGroup "daemon mode" []


parallelization :: TestTree
parallelization = testGroup "parallelization" []


assertNotEqual :: (Eq a, Show a) => String -- ^ The message prefix
                              -> a      -- ^ The expected value
                              -> a      -- ^ The actual value
                              -> Assertion
assertNotEqual preface expected actual =
  unless (actual /= expected) (assertFailure msg)
    where
      msg =
        (if null preface then "" else preface ++ "\n")
        ++ "expected not to get: " ++ show expected ++ "\n but got: " ++ show actual
