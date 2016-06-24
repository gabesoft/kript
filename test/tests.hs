-- | Main test suite
import Test.Tasty
import qualified Tests.Gown.Parser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Tests.Gown.Parser.tests]