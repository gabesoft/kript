-- | Tests for Gown.Processor module
module Tests.Gown.Processor where

import Control.Monad (join)
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Gown.Parser (AclEntry(..), AclOwners(..))
import Gown.Processor
import Test.HUnit (Assertion, (@?=))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck
       (testProperty, elements, listOf1, Arbitrary(..), Property, (==>))
import Tests.Gown.TestData

bestGroupTest :: Assertion
bestGroupTest = sort (bestGroup files owners) @?= expected
  where files = group ['a' .. 'e']
        owners =
          [("u1",["a","c"])
          ,("u2",["a"])
          ,("u3",["b","d"])
          ,("u4",["b","c","d"])
          ,("u5",["b","d","e"])]
        expected = ["u1","u5"]

findAclGroupsTest1 :: Assertion
findAclGroupsTest1 = actual @?= expected
  where actual = findAclGroups 5 aclEntriesTiny
        expected = [["a","c"],["a","d"],["a","e"],["a","f"]]

findAclGroupsTest2 :: Assertion
findAclGroupsTest2 = actual @?= expected
  where actual = findAclGroups 3 aclEntriesLarge
        expected =
          [["u1","u2","u20","u5"]
          ,["u1","u10","u18","u20","u3","u5"]
          ,["u1","u10","u18","u20","u4","u5"]]

findAclGroupsTest3 :: Assertion
findAclGroupsTest3 = actual @?= expected
  where actual = findAclGroups 10 aclEntriesSmall
        expected =
          [["q1","u1"]
          ,["q2","u1"]
          ,["n1","u1","w1"]
          ,["n2","u1","w1"]
          ,["n3","u1","w1"]
          ,["n3","u1","w2"]
          ,["n3","u1","w3"]
          ,["n3","u2","w3"]
          ,["k1","n3","u3","w3"]
          ,["n3","s1","u3","w3"]]

bestGroupPerf :: Assertion
bestGroupPerf = sort (bestGroup files owners) @?= expected
  where files = map aclFile aclEntriesLarge
        owners = filesByOwner aclEntriesLarge
        expected = ["u1","u2","u20","u5"]

bestGroupNotAllFilesCovered :: Assertion
bestGroupNotAllFilesCovered = sort (bestGroup files owners) @?= expected
  where files = group ('-' : ['a' .. 'z'])
        owners = filesByOwner aclEntriesLarge
        expected = []

filesByOwner entries = toAlist $ mkReverseMap ownersByFile
  where ownersByFile = map (toTuple aclFile owners) entries
        owners = join . map aclNames . aclOwners

pruneSimilarTest :: Assertion
pruneSimilarTest = (Set.toList actual) @?= expected
  where alist =
          [("u1",["a","c","x"])
          ,("u2",["a","c"])
          ,("u3",["a","c","z"])
          ,("u4",["a","c"])
          ,("u5",["a","c","w","x"])
          ,("u6",["a","c"])
          ,("u7",["a","c"])
          ,("u8",["a","b"])
          ,("u9",["k","b"])]
        expected = ["u1","u2","u3","u5","u8","u9"]
        actual =
          pruneSimilar (Set.fromList $ map fst alist)
                       (mkMap alist)

excludeAllTest :: Assertion
excludeAllTest = excludeAll keys alist @?= expected
  where keys = "abcd"
        alist = [(x,y)|x <- "abf",y <- [1 .. 2]]
        expected = [('f',1),('f',2)]

aclTypesByOwnerTest :: Assertion
aclTypesByOwnerTest = actual @?= expected
  where entries = aclEntriesSmall
        actual = aclTypesByOwner entries
        expected =
          [("k1",["t3"])
          ,("n1",["t6"])
          ,("n2",["t6"])
          ,("n3",["t6"])
          ,("q1",["t7","t8"])
          ,("q2",["t7","t8"])
          ,("s1",["t9"])
          ,("u1",["t1"])
          ,("u2",["t1","t9"])
          ,("u3",["t1"])
          ,("v1",["t2"])
          ,("v2",["t2"])
          ,("w1",["t5"])
          ,("w2",["t5"])
          ,("w3",["t5"])
          ,("y1",["t4"])
          ,("y2",["t4"])]

unitTests :: TestTree
unitTests =
  testGroup "Unit Tests"
            [testCase "bestGroup" bestGroupTest
            ,testCase "bestGroup - performance" bestGroupPerf
            ,testCase "bestGroup - not all files covered" bestGroupNotAllFilesCovered
            ,testCase "pruneSimilar" pruneSimilarTest
            ,testCase "aclTypesByOwner" aclTypesByOwnerTest
            ,testCase "findAclGroups - 1" findAclGroupsTest1
            ,testCase "findAclGroups - 2" findAclGroupsTest2
            ,testCase "findAclGroups - 3" findAclGroupsTest3
            ,testCase "excludeAll" excludeAllTest]

propTests :: TestTree
propTests = testGroup "Property Tests" []

tests :: TestTree
tests = testGroup "Gown.Processor" [unitTests,propTests]