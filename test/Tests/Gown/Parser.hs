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
         elements $
         ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "$-_.!*'()+&|\\?"
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

mkEntry (file,owners) = AclEntry file (map mkOwners owners)
  where mkOwners (aclType,names) = AclOwners aclType names

fileOwnersSingle = makeParseTest fileOwners input output
  where input = " a/b/c/file1.txt:\n   type.acl: joe, jane, mo\n"
        output = mkEntry ("a/b/c/file1.txt",[("type.acl",["joe","jane","mo"])])

fileOwnersMultiple =
  makeParseTest parser
                input
                (map mkEntry output)
  where parser = many fileOwners
        output =
          [("a/b/file1.txt",[("acl1",["u1","u2","u3"])])
          ,("a/b/file2.txt",[("acl2",["v1","u2","v3"])])
          ,("a/b/file3.txt",[("acl3",["k1","u2","v3"])])]
        input =
          intercalate
            "\n"
            [" a/b/file1.txt:"
            ,"  acl1: u1, u2, u3"
            ," a/b/file2.txt:"
            ,"  acl2: v1, u2, v3"
            ," a/b/file3.txt:"
            ,"  acl3: k1, u2, v3"
            ,""]

aclSingle = makeParseTest acl input output
  where input = "  aname: blue, green, pink\n"
        output = AclOwners "aname" ["blue","green","pink"]

unitTests :: TestTree
unitTests =
  testGroup "Gown.Parser - Unit Tests"
            [testCase "name" $ makeParseTest name "dagny" "dagny"
            ,testCase "names" $
             makeParseTest names
                           "blue, red, green"
                           ["blue","red","green"]
            ,testCase "filePath" $
             makeParseTest filePath " a/b/c/file1.txt:\n" "a/b/c/file1.txt"
            ,testCase "fileOwners - single" fileOwnersSingle
            ,testCase "fileOwners - multiple" fileOwnersMultiple
            ,testCase "acl" aclSingle
            ,testCase "aclName" $ makeParseTest aclName " dyno:" "dyno"]

tests :: TestTree
tests = testGroup "Gown.Parser" [unitTests,propTests]