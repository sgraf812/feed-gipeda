module Unit
  ( tests
  ) where


import           Test.Tasty
import qualified Unit.CommitQueue


tests :: TestTree
tests = testGroup "Unit tests"
  [ Unit.CommitQueue.tests
  ]
