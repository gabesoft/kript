-- | Tests for Gown.Parser
module Tests.Gown.Parser (tests) where

import Data.List
import Gown.Parser
import Test.HUnit (Assertion, (@?=))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck
       (testProperty, elements, listOf1, Arbitrary(..), Property, (==>))
import Text.ParserCombinators.Parsec

newtype SafeName =
  SafeName {safeName :: String}
  deriving (Show,Eq)

instance Arbitrary SafeName where
  arbitrary =
    do name <-
         listOf1 $
         elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "_-.!*|+&\\"
       return $ SafeName name

makeParseTest
  :: (Eq a,Show a)
  => CharParser () a -> String -> a -> Assertion
makeParseTest parser input output = actual @?= expected
  where actual = parse parser "" input
        expected = Right output

namesProp :: [SafeName] -> Property
namesProp xs = cond ==> actual == Right (map safeName xs)
  where actual = parse names "" str
        str =
          intercalate ", "
                      (map safeName xs)
        cond = length xs > 0

propTests :: TestTree
propTests =
  testGroup "Gown.Parser - Property Tests" [testProperty "names" namesProp]

unitTests :: TestTree
unitTests =
  testGroup "Gown.Parser - Unit Tests"
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

tests :: TestTree
tests = testGroup "Gown.Parser" [unitTests,propTests]