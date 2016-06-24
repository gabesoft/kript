-- | Tests for Gown.Parser
module Tests.Gown.Parser (tests) where

import Gown.Parser
import Test.HUnit (Assertion, (@?=))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim (ParsecT)
import Data.Functor.Identity (Identity)

makeParseTest
  :: (Eq a,Show a)
  => ParsecT String () Identity a -> String -> a -> Assertion
makeParseTest parser input output = actual @?= expected
  where actual = parse parser "" input
        expected = Right output

-- TODO: add a quickcheck test for names

tests :: TestTree
tests =
  testGroup "Gown.Parser"
            [testCase "name" $ makeParseTest name "dagny" "dagny"
            ,testCase "names" $
             makeParseTest names
                           "blue, red, green"
                           ["blue","red","green"]
            ,testCase "filePath" $
             makeParseTest filePath " a/b/c/file1.txt:" "a/b/c/file1.txt"
            ,testCase "acl" $
             makeParseTest acl
                           "  aname: blue, green, pink\n"
                           ("aname",["blue","green","pink"])
            ,testCase "aclName" $ makeParseTest aclName " dyno:" "dyno"]