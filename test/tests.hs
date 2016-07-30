-- | Main test suite
import Test.Tasty
import qualified Tests.Gown.Parser
import qualified Tests.Gown.Processor

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Tests.Gown.Parser.tests,Tests.Gown.Processor.tests]
