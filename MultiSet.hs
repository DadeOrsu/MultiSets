module MultiSet
  ( MSet(..)
  , empty
  , add
  , occs
  , elems
  , subeq
  , union
  , mapMSet
  ) where

data MSet a = MS [(a, Int)] deriving (Show)

-- Constructor for an empty multiset
empty :: MSet a
empty = MS []

-- Add an element to the multiset
add :: Eq a => MSet a -> a -> MSet a
add (MS ms) v = MS (addHelper ms v)
  where
    addHelper :: Eq a => [(a, Int)] -> a -> [(a, Int)]
    addHelper [] newV = [(newV, 1)]
    addHelper ((existingV, n):rest) newV
      | existingV == newV = (existingV, n + 1) : rest
      | otherwise = (existingV, n) : addHelper rest newV

-- Count the occurrences of an element in the multiset
occs :: Eq a => MSet a -> a -> Int
occs (MS ms) v = occsHelper ms v
  where
    occsHelper :: Eq a => [(a, Int)] -> a -> Int
    occsHelper [] _ = 0
    occsHelper ((x, n):rest) target
      | x == target = n
      | otherwise = occsHelper rest target

-- Get a list of all elements in the multiset
elems :: MSet a -> [a]
elems (MS ms) = concatMap (\(x, _) -> [x]) ms

-- Check if mset1 is a submultiset of mset2
subeq :: Eq a => MSet a -> MSet a -> Bool
subeq (MS ms1) (MS ms2) = all (\(v, n) -> n <= findMultiplicity v ms2) ms1
  where
    findMultiplicity :: Eq a => a -> [(a, Int)] -> Int
    findMultiplicity _ [] = 0
    findMultiplicity target ((x, m):rest)
      | target == x = m
      | otherwise = findMultiplicity target rest

-- Union of two multisets
union :: Eq a => Ord a => MSet a -> MSet a -> MSet a
union (MS ms1) (MS ms2) = MS (unionHelper ms1 ms2)
  where
    unionHelper :: Eq a => Ord a => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
    unionHelper [] ms = ms
    unionHelper ms [] = ms
    unionHelper ((v1, n1):rest1) ((v2, n2):rest2)
      | v1 == v2 = (v1, n1 + n2) : unionHelper rest1 rest2
      | v1 < v2 = (v1, n1) : unionHelper rest1 ((v2, n2):rest2)
      | otherwise = (v2, n2) : unionHelper ((v1, n1):rest1) rest2

-- Equality instance for MSet
instance Eq a => Eq (MSet a) where
  (==) mset1 mset2 = subeq mset1 mset2 && subeq mset2 mset1

-- Foldable instance for MSet
instance Foldable MSet where
  foldr :: (a -> b -> b) -> b -> MSet a -> b
  foldr f acc (MS ms) = foldr (\(v, _) restAcc -> f v restAcc) acc ms

-- Map function for MSet
mapMSet :: (a -> b) -> MSet a -> MSet b
mapMSet f (MS ms) = MS (map (\(v, n) -> (f v, n)) ms)

{-
  It's not possible to define a lawful instance of Functor for MSet directly using fmap, 
  because fmap should obey the Functor laws, particularly the preservation of the identity and composition.

  For MSet, the functorial mapping should not only transform the elements of the multiset but also
  consider the multiplicities associated with each element. 

  Functor laws require that fmap id = id and fmap (g . f) = fmap g . fmap f. However, since MSet is a multiset
  and not a traditional container type, applying fmap directly would lead to violations of these laws.

  Specifically, fmap id should result in the original multiset, preserving multiplicities, 
  but applying id to each element may change its multiplicity, violating the law.
  
  For example, fmap id (MS [(1, 2), (2, 3)]) would result in MS [(1, 1), (2, 1)], which is not equal to the original multiset.

  Additionally, fmap (g . f) should be equivalent to composing the individual mappings, 
  but directly applying fmap to MSet would not consider the interplay of multiplicities and might produce incorrect results.
-}

