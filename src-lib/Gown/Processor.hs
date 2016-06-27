-- | Utilities for processing a list of acl entries
module Gown.Processor
       (findAclGroups, aclTypesByOwner, excludeAll, sortByLongestValues,
        bestGroup)
       where

import Control.Monad
import Data.List (sortBy, minimumBy)
import Data.Ord (comparing)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Gown.Parser

sampleData :: IO [AclEntry]
sampleData = fmap extractSample parseAclsSample
  where extractSample (Right xs) = xs
        extractSample _ = []

-- | Given a list of acl entries find all groups such that the members of
-- | each group collectively own all files in the input entries list
findAclGroups :: [AclEntry] -> [[String]]
findAclGroups entries = findAllGroups allFiles ownersAlist
  where ownersMap = mkFilesByOwnerMap entries
        ownersAlist = sortByLongestValues $ Map.toList ownersMap
        allFiles = map aclFile entries

-- | Returns an an association list matching owners with the comprehensive
-- | list of acl types that they belong to
aclTypesByOwner :: [AclEntry] -> [(String,[String])]
aclTypesByOwner = aclsByOwner . mkAclsByOwnerMap

aclsByOwner
  :: Map.Map String [String] -> [(String,[String])]
aclsByOwner aclMap = sortBy ownerName owners
  where owners = Map.toList aclMap
        ownerName (n1,_) (n2,_) = compare n1 n2

mkFilesByOwnerMap
  :: [AclEntry] -> Map.Map String [String]
mkFilesByOwnerMap = foldr addOwners Map.empty
  where addOwners entry ownerMap =
          let addToMap = addToOwnerMap aclFile entry
              owners = join . map aclNames $ aclOwners entry
          in foldr addToMap ownerMap owners

mkAclsByOwnerMap
  :: [AclEntry] -> Map.Map String [String]
mkAclsByOwnerMap = foldr addOwners Map.empty
  where addOwners entry ownerMap = foldr addAcls ownerMap (aclOwners entry)
        addAcls owners nextMap =
          let addToMap = addToOwnerMap aclType owners
          in foldr addToMap nextMap (aclNames owners)

addToOwnerMap
  :: (Ord k,Ord a)
  => (t -> a) -> t -> k -> Map.Map k [a] -> Map.Map k [a]
addToOwnerMap item entry owner ownerMap =
  case Map.lookup owner ownerMap of
    Nothing ->
      Map.insert owner
                 [item entry]
                 ownerMap
    Just items ->
      Map.insert owner
                 (dedup $ item entry : items)
                 ownerMap

findAllGroups
  :: [String] -> [(String,[String])] -> [[String]]
findAllGroups files owners = loop [] files owners
  where loop groups remainingFiles remainingOwners =
          case bestGroup remainingFiles remainingOwners of
            [] -> reverse groups
            gs ->
              loop (gs : groups)
                   files
                   (excludeAll gs remainingOwners)

-- | Exclude all entries in an association list whose keys are members of the given key list
excludeAll :: (Eq a)
           => [a] -> [(a,b)] -> [(a,b)]
excludeAll keys = filter $ (not . flip elem keys) . fst

-- | Given a list of files and an association list mapping owners to files
-- | find the smallest group of owners that collectively own all specified files
-- | This is in essence a brute force implementation of the set cover problem
bestGroup :: (Ord a,Ord b)
          => [b] -> [(a,[b])] -> [a]
bestGroup files owners = loop filesSet []
  where filesSet = Set.fromList files
        filesByOwner = mkMap owners
        ownersByFile = mkReverseMap owners
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

mkMap :: (Ord a,Ord b)
      => [(a,[b])] -> Map.Map a (Set.Set b)
mkMap alist = Map.fromList (map (sndMap Set.fromList) alist)

mkReverseMap
  :: (Ord a,Ord b)
  => [(a,[b])] -> Map.Map b (Set.Set a)
mkReverseMap = foldr add Map.empty
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

-- | Order an association list by the length of its values, longest first
sortByLongestValues :: [(a,[b])] -> [(a,[b])]
sortByLongestValues = sortBy (flip $ comparing $ length . snd)

dedup :: (Ord a)
      => [a] -> [a]
dedup = Set.toList . Set.fromList