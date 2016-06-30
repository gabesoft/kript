-- | Utilities for processing a list of acl entries
module Gown.Processor
       (findAclGroups, aclTypesByOwner, excludeAll, bestGroup,
        pruneSimilar, toMap, toReverseMap, toTuple, toAlist)
       where

import Control.Monad
import Control.Monad.State
import Data.List (sort, sortBy, groupBy, minimumBy, transpose)
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
  findBestGroups maxResults $ (toAlist . toReverseMap) ownersByFile
  where ownersByFile = map (toTuple aclFile toOwners) entries
        toOwners = join . map aclNames . aclOwners

-- | Returns an an association list matching owners with a
-- | list of acl types that they belong to
aclTypesByOwner
  :: [AclEntry] -> [(String,[String])]
aclTypesByOwner entries = sortBy (comparing fst) typesByOwner
  where ownersByType = map (toTuple aclType aclNames) $ extractOwners entries
        extractOwners = join . map aclOwners
        typesByOwner = (toAlist . toReverseMap) ownersByType

-- | Given an association list of keys to lists of values
-- | find the smallest groups of keys such that the members of
-- | each group collectively account for all values
findBestGroups :: (Ord a,Ord b)
               => Int -> [(a,[b])] -> [[a]]
findBestGroups limit alist = map sort $ sortBy (comparing length) groups
  where values = dedup $ join $ map snd alist
        groups =
          evalState (findGroups alist)
                    (Set.empty,values,limit)

-- | Helper function for 'findBestGroups'
findGroups
  :: (Num t,Eq t,Ord a,Ord b)
  => [(a,[b])] -> State (Set.Set [a],[b],t) [[a]]
findGroups alist =
  do (seen,vs,limit) <- get
     let keys = map fst alist
     case (Set.member keys seen,limit) of
       (True,_) -> return []
       (_,0) -> return []
       (False,_) ->
         do put (Set.insert keys seen,vs,updateLimit group)
            rest <- mapM findGroups nextAlist
            let cont = interleave rest
            return $
              case group of
                [] -> cont
                _ -> group : cont
         where group = bestGroup vs alist
               nextAlist = map (removeByFst alist) group
               updateLimit [] = limit
               updateLimit xs = limit - 1

-- | Given a list of files and an association list mapping owners to files
-- | find the smallest group of owners that collectively own all specified files
-- | This is in essence a brute force implementation of the set cover problem
bestGroup :: (Ord a,Ord b)
          => [b] -> [(a,[b])] -> [a]
bestGroup files owners
  | filesSet == actualFiles = loop relevantFiles []
  | otherwise = []
  where filesByOwner = toMap owners
        filesSet = Set.fromList files
        relevantUsers =
          pruneSimilar (Set.fromList $ map fst owners)
                       filesByOwner
        ownersByFile =
          Map.map (`Set.intersection` relevantUsers) (toReverseMap owners)
        actualFiles = Set.fromList (Map.keys ownersByFile)
        relevantFiles = pruneSimilar filesSet ownersByFile
        exclude owner remaining =
          case Map.lookup owner filesByOwner of
            Nothing -> Set.empty
            Just fs -> Set.difference remaining fs
        loop remaining best
          | Set.null remaining = best
          | otherwise =
            let file = Set.elemAt 0 remaining
            in case Map.lookup file ownersByFile of
                 Nothing -> []
                 Just os ->
                   minimumBy (comparing length) $
                   map (\o ->
                          loop (exclude o remaining)
                               (o : best))
                       (Set.toList os)

-- | Given a set of keys and a map containing the same keys
-- | keep only items in the keys set that have different values
-- | in the map
pruneSimilar
  :: (Eq a,Ord a,Ord b)
  => Set.Set b -> Map.Map b (Set.Set a) -> Set.Set b
pruneSimilar xset xmap =
  Set.difference xset
                 (Set.fromList remove)
  where alist = sortBy (comparing snd) $ Map.toList xmap
        groups = groupBy (\x y -> (snd x) == (snd y)) alist
        remove = join $ map (tail . map fst) groups

-- | Exclude all entries in an association list whose keys are members of the given key list
excludeAll :: (Eq a)
           => [a] -> [(a,b)] -> [(a,b)]
excludeAll keys = filter $ (not . flip elem keys) . fst

toMap :: (Ord a,Ord b)
      => [(a,[b])] -> Map.Map a (Set.Set b)
toMap alist = Map.fromList (map (sndMap Set.fromList) alist)

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

toTuple a b x = (a x,b x)

dedup :: (Ord a)
      => [a] -> [a]
dedup = Set.toList . Set.fromList

interleave :: [[a]] -> [a]
interleave = concat . transpose

removeByFst :: (Eq a)
            => [(a,b)] -> a -> [(a,b)]
removeByFst [] _ = []
removeByFst (x:xs) y
  | fst x == y = xs
  | otherwise = x : removeByFst xs y