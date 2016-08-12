import qualified Acceptance
import           Test.Tasty
import qualified Unit


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Acceptance.tests, Unit.tests]
