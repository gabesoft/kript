-- | Main test suite
import Test.Tasty
import qualified Tests.Gown.Parser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = Tests.Gown.Parser.tests