-- | Utilities for processing a list of acl entries
module Gown.Processor
       (findAclGroups, aclTypesByOwner, bestGroup, pruneSimilar, toMap,
        toReverseMap, toTuple, toAlist)
       where

import Control.Monad
import Control.Monad.State
import Data.List (sort, sortBy, minimumBy, transpose)
import qualified Data.Map as Map
import Data.Ord (comparing)
import qualified Data.Set as Set
import Gown.Parser

sampleData :: IO [AclEntry]
sampleData = fmap extractSample parseAclsSample
  where extractSample (Right xs) = xs
        extractSample _ = []

-- | Given a list of acl entries find all groups such that the members of
-- | each group collectively own all files in the input entries list
findAclGroups :: Int -> [AclEntry] -> [[String]]
findAclGroups maxResults entries =
  ownersByFile & toReverseMap & toAlist & findBestGroups maxResults
  where ownersByFile = map (toTuple aclFile toOwners) entries
        toOwners = join . map aclNames . aclOwners

-- | Returns an an association list matching owners with a
-- | list of acl types that they belong to
aclTypesByOwner
  :: [AclEntry] -> [(String,[String])]
aclTypesByOwner entries = sortBy (comparing fst) typesByOwner
  where typesByOwner = ownersByType & toReverseMap & toAlist
        ownersByType = entries & map aclOwners & join & mapByType
        mapByType = map (toTuple aclType aclNames)

-- | Given an association list of keys to lists of values
-- | find the smallest groups of keys such that the members of
-- | each group collectively account for all values
findBestGroups :: (Ord a,Ord b)
               => Int -> [(a,[b])] -> [[a]]
findBestGroups limit alist = groups & sortBy (comparing length) & map sort
  where groups =
          evalState (findGroups alist)
                    (Set.empty,valuesToSet alist,limit)

-- | Helper function for 'findBestGroups'
findGroups
  :: (Num t,Eq t,Ord a,Ord b)
  => [(a,[b])] -> State (Set.Set [a],Set.Set b,t) [[a]]
findGroups alist =
  do (seen,vals,limit) <- get
     let keys = map fst alist
     case (Set.member keys seen,limit) of
       (True,_) -> return []
       (_,0) -> return []
       (False,_) ->
         do put (Set.insert keys seen,vals,nextLimit group)
            rest <- mapM findGroups (nextAlist group)
            let cont = interleave rest
            return $
              case group of
                [] -> cont
                _ -> group : cont
         where group
                 | valuesToSet alist == vals = bestGroup alist
                 | otherwise = []
               nextAlist = map (removeByFst alist)
               nextLimit xs
                 | null xs = limit
                 | otherwise = limit - 1

-- | Given an association list of keys to lists of values
-- | find the smallest group of keys such that the members of
-- | each group collectively account for all values
-- | This is in essence a brute force implementation of the set cover problem
bestGroup :: (Ord a,Ord b)
          => [(a,[b])] -> [a]
bestGroup alist = loop initialValues []
  where valuesByKey = toMap alist
        keysByValue =
          Map.map (`Set.intersection` initialKeys) (toReverseMap alist)
        initialKeys = pruneSimilar valuesByKey
        initialValues = pruneSimilar keysByValue
        excludeValues key inputValues =
          maybe Set.empty
                (Set.difference inputValues)
                (Map.lookup key valuesByKey)
        loop valuesLeft best
          | Set.null valuesLeft = best
          | otherwise =
            case Map.lookup (Set.elemAt 0 valuesLeft)
                            keysByValue of
              Nothing -> []
              Just matchingKeys ->
                minimumBy (comparing length) $
                map (\key ->
                       loop (excludeValues key valuesLeft)
                            (key : best))
                    (Set.toList matchingKeys)

-- | Given a hash map of keys and values where some keys could
-- | map to identical values, return a set of keys that map to
-- | distinct values
pruneSimilar
  :: (Eq a,Ord a,Ord b)
  => Map.Map b (Set.Set a) -> Set.Set b
pruneSimilar inputMap =
  fst $
  Map.foldlWithKey addKeys
                   (Set.empty,Set.empty)
                   inputMap
  where addKeys (kacc,vacc) key value
          | Set.member value vacc = (kacc,vacc)
          | otherwise = (Set.insert key kacc,Set.insert value vacc)

-- | Convert an association list of keys mapping to lists of values into a hash map
-- | where the keys map to sets of values
toMap :: (Ord a,Ord b)
      => [(a,[b])] -> Map.Map a (Set.Set b)
toMap alist = Map.fromList (map (sndMap Set.fromList) alist)

-- | Convert an association list of keys mapping to lists of values into a hash map
-- | where the values map to sets of keys
toReverseMap
  :: (Ord a,Ord b)
  => [(a,[b])] -> Map.Map b (Set.Set a)
toReverseMap = foldr add Map.empty
  where add (a,bs) acc = foldr addNested acc bs
          where addNested b nestedAcc =
                  case Map.lookup b nestedAcc of
                    Nothing ->
                      Map.insert b
                                 (Set.singleton a)
                                 nestedAcc
                    Just as ->
                      Map.insert b
                                 (Set.insert a as)
                                 nestedAcc

-- | Map the second value of a tuple
sndMap :: (a -> b) -> (c,a) -> (c,b)
sndMap f (a,b) = (a,f b)

-- | Convert a hash map of sets to an association list
toAlist :: Map.Map a (Set.Set b) -> [(a,[b])]
toAlist = Map.toList . (Map.map Set.toList)

toTuple :: (a -> b) -> (a -> c) -> a -> (b,c)
toTuple a b x = (a x,b x)

valuesToSet :: (Ord b)
            => [(a,[b])] -> Set.Set b
valuesToSet = Set.fromList . join . map snd

(&) :: a -> (a -> c) -> c
(&) = flip ($)

interleave :: [[a]] -> [a]
interleave = concat . transpose

removeByFst :: (Eq a)
            => [(a,b)] -> a -> [(a,b)]
removeByFst [] _ = []
removeByFst (x:xs) y
  | fst x == y = xs
  | otherwise = x : removeByFst xs y