-- | Tests for Gown.Processor module
module Tests.Gown.Processor where

import Data.List
import qualified Data.Set as Set
import Gown.Processor
import Test.HUnit (Assertion, (@?=))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck
       (testProperty, elements, listOf1, Arbitrary(..), Property, (==>))

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
        expected = ["u1", "u2","u3","u5","u8","u9"]
        actual =
          pruneSimilar (Set.fromList $ map fst alist)
                       (mkMap alist)

filesByOwnerTest :: Assertion
filesByOwnerTest = undefined

excludeAllTest :: Assertion
excludeAllTest = excludeAll keys alist @?= expected
  where keys = "abcd"
        alist = [(x,y)|x <- "abf",y <- [1 .. 2]]
        expected = [('f',1),('f',2)]

sortByLongestValuesProp :: [(Int,[Int])] -> Bool
sortByLongestValuesProp alist =
  map (length . snd) sorted == (reverse . sort) (map (length . snd) alist)
  where sorted = sortByLongestValues alist

unitTests :: TestTree
unitTests =
  testGroup "Unit Tests"
            [testCase "bestGroup" bestGroupTest
            ,testCase "bestGroup - performance" bestGroupPerf
            ,testCase "bestGroup - not all files covered" bestGroupNotAllFilesCovered
            ,testCase "pruneSimilar" pruneSimilarTest
            ,testCase "excludeAll" excludeAllTest]

propTests :: TestTree
propTests =
  testGroup "Property Tests" [testProperty "sortByLongestValues" sortByLongestValuesProp]

tests :: TestTree
tests = testGroup "Gown.Processor" [unitTests,propTests]

bestGroupPerf :: Assertion
bestGroupPerf = sort (bestGroup files owners) @?= expected
  where files = group ['a' .. 'z']
        owners =
          [("u1",["a","b","x","w","z"])
          ,("u2",["a","b","u","v","w"])
          ,("u3",["a","b","c"])
          ,("u4",["a","b","c","d"])
          ,("u3a",["a","b"])
          ,("u3b",["a","b"])
          ,("u3c",["a","b"])
          ,("u5",["a","b","c","d","e","y"])
          ,("u6",["a","b","c","d","e","f"])
          ,("u7",["a","b","c","d","e","f","g"])
          ,("u8",["a","b","c","d","e","f","g","h"])
          ,("u9",["a","b","c","d","e","f","g","h","i"])
          ,("u10",["a","b","c","d","e","f","g","h","i","j"])
          ,("u11",["a","b","c","d","e","f","g","h","i","j","k"])
          ,("u12",["a","b","c","d","e","f","g","h","i","j","k","l"])
          ,("u13",["a","b","c","d","e","f","g","h","i","j","k","l","m"])
          ,("u14",["a","b","c","d","e","f","g","h","i","j","k","l","m","n"])
          ,("u15",["a","b","c","d","e","f","g","h","l","m","n","o"])
          ,("u16"
           ,["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p"])
          ,("u17",["a","b","h","i","j","k","l","m","n","o","p","q"])
          ,("u18",["a","b","g","h","i","j","k","l","m","n","o","p","q","r"])
          ,("u19",["a","b","h","i","j","k","l","m","n","o","p","q","r","s"])
          ,("u20",["a","b","c","d","h","i","j","k","l","m","n","o","s","t"])]
        expected = ["u1","u10","u18","u2","u20","u5"]

bestGroupNotAllFilesCovered :: Assertion
bestGroupNotAllFilesCovered = sort (bestGroup files owners) @?= expected
  where files = group ['a' .. 'z']
        owners =
          [("u1",["a","x","w","z"])
          ,("u2",["a","b","u"])
          ,("u3",["a","b","c"])
          ,("u3a",["a","b","c"])
          ,("u3b",["a","b","c"])
          ,("u3c",["a","b","c"])
          ,("u4",["a","b","c","d"])
          ,("u5",["a","b","c","d","e","y"])
          ,("u6",["a","b","c","d","e","f"])
          ,("u7",["a","b","c","d","e","f","g"])
          ,("u8",["a","b","c","d","e","f","g","h"])
          ,("u9",["a","b","c","d","e","f","g","h","i"])
          ,("u10",["a","b","c","d","e","f","g","h","i","j"])
          ,("u11",["a","b","c","d","e","f","g","h","i","j","k"])
          ,("u12",["a","b","c","d","e","f","g","h","i","j","k","l"])
          ,("u13",["a","b","c","d","e","f","g","h","i","j","k","l","m"])
          ,("u14",["a","b","c","d","e","f","g","h","i","j","k","l","m","n"])
          ,("u15",["a","b","c","d","e","f","g","h","l","m","n","o"])
          ,("u16"
           ,["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p"])
          ,("u17",["h","i","j","k","l","m","n","o","p","q"])
          ,("u18",["a","b","g","h","i","j","k","l","m","n","o","p","q","r"])
          ,("u19",["a","b","h","i","j","k","l","m","n","o","p","q","r","s"])
          ,("u20",["a","b","c","d","h","i","j","k","l","m","n","o","s","t"])]
        expected = []