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

-- |Constructor for an empty multiset
empty :: MSet a
empty = MS []

-- |returns a multiset obtained by adding an element v to the multiset
add :: Eq a => MSet a -> a -> MSet a
add (MS ms) v = MS (addHelper ms v)
  where
    addHelper :: Eq a => [(a, Int)] -> a -> [(a, Int)]
    addHelper [] newV = [(newV, 1)]
    addHelper ((existingV, n):rest) newV
      | existingV == newV = (existingV, n + 1) : rest
      | otherwise = (existingV, n) : addHelper rest newV

-- |returns the number of occurrences of v in mset
occs :: Eq a => MSet a -> a -> Int
occs (MS ms) v = occsHelper ms v
  where
    occsHelper :: Eq a => [(a, Int)] -> a -> Int
    occsHelper [] _ = 0
    occsHelper ((x, n):rest) v
      | x == v = n
      | otherwise = occsHelper rest v

-- |returns a list containing the elements of the multiset
elems :: MSet a -> [a]
elems (MS ms) = concatMap (\(x, _) -> [x]) ms

-- |returns True if each element of MSet a is also an element
-- of MSet b with the same multiplicity at least.
subeq :: Eq a => MSet a -> MSet a -> Bool
subeq (MS ms1) (MS ms2) = all (\(v, n) -> n <= occs (MS ms2) v) ms1

-- |returns a MSet having all the elements of mset1 and mset2
-- each with the sum of their multiplicities in mset1 and mset2
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

-- |Equality instance for MSet
instance Eq a => Eq (MSet a) where
  (==) :: Eq a => MSet a -> MSet a -> Bool
  (==) mset1 mset2 = subeq mset1 mset2 && subeq mset2 mset1

-- |Foldable instance for MSet
instance Foldable MSet where
  foldr :: (a -> b -> b) -> b -> MSet a -> b
  foldr f acc (MS ms) = foldr (\(v, _) restAcc -> f v restAcc) acc ms

-- |Map function for MSet
-- NOTE:  it is not possible an instance of Functor for MSet
--        because (according to the documentation) a type f is a 
--        Functor if it provides a function fmap which, given any types a and b,
--        lets you apply any function of type (a -> b) to turn an f a into an f b,
--        preserving the structure of f.
--        The fact is that mapMSet is designed to be used with MSets, not with
--        any type a and b.
--        
mapMSet :: (a -> b) -> MSet a -> MSet b
mapMSet f (MS ms) = MS (map (\(v, n) -> (f v, n)) ms)
