import qualified Acceptance
import           Test.Tasty


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Acceptance.tests]
