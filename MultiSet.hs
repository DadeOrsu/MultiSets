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
import Data.Maybe ( fromMaybe )
import Data.List (sortBy, sort)

data MSet a = MS [(a, Int)] deriving (Show)

{-|Constructor for an empty multiset
-}
empty :: MSet a
empty = MS []

{-|returns a multiset obtained by adding an element v to the multiset

- @ms@ is the MSet to be added to
- @v@ is the element to be added
-}
add :: Eq a => MSet a -> a -> MSet a
add (MS ms) v = MS (addHelper ms v)
  where
    addHelper :: Eq a => [(a, Int)] -> a -> [(a, Int)]
    addHelper ms' v' = case lookup v' ms' of
      Just n  -> map (\(x, m) -> if x == v' then (x, m + 1) else (x, m)) ms'
      Nothing -> (v', 1) : ms'

{-|returns the number of occurrences of v in mset

- @ms@ is the MSet to be checked
- @v@ is the element to be checked
-}
occs :: Eq a => MSet a -> a -> Int
occs (MS ms) v = fromMaybe 0 (lookup v ms)

{-|returns a list containing the elements of the multiset
- @ms@ is the MSet to be converted to a list
-}
elems :: MSet a -> [a]
elems (MS ms) = concatMap (\(x, _) -> [x]) ms

{-|returns True if each element of MSet a is also an element
of MSet b with the same multiplicity at least.
- @ms1@ is the the first MSet to be checked
- @ms2@ is the second MSet to be checked
-}
subeq :: Eq a => MSet a -> MSet a -> Bool
subeq (MS ms1) (MS ms2) = all (\(v, n) -> n <= occs (MS ms2) v) ms1

{-|returns a MSet having all the elements of mset1 and mset2
each with the sum of their multiplicities in mset1 and mset2
- @ms1@ is the first MSet to be united
- @ms2@ is the second MSet to be united
-}

union :: Eq a => Ord a => MSet a -> MSet a -> MSet a
union (MS ms1) (MS ms2) = MS $ unionHelper (sortPairs $ MS ms1) (sortPairs $ MS ms2)
  where
    unionHelper :: Eq a => Ord a =>[(a, Int)] -> [(a, Int)] -> [(a, Int)]
    unionHelper [] ms = ms
    unionHelper ms [] = ms
    unionHelper ((v1, n1):rest1) ((v2, n2):rest2)
      | v1 == v2 = (v1, n1 + n2) : unionHelper rest1 rest2
      | v1 < v2 = (v1, n1) : unionHelper rest1 ((v2, n2):rest2)
      | otherwise = (v2, n2) : unionHelper ((v1, n1):rest1) rest2

    sortPairs :: Eq a => Ord a => MSet a -> [(a, Int)]
    sortPairs (MS ms) = sort ms


{-|Equality instance for MSet
-}
instance Eq a => Eq (MSet a) where
  (==) :: Eq a => MSet a -> MSet a -> Bool
  (==) mset1 mset2 = subeq mset1 mset2 && subeq mset2 mset1

{-|Foldable instance for MSet
-}
instance Foldable MSet where
  foldr :: (a -> b -> b) -> b -> MSet a -> b
  foldr f acc (MS ms) = foldr (\(v, _) restAcc -> f v restAcc) acc ms


{-| mapMSet f mset returns a multiset obtained by applying f to each element of mset
- @f@ is the function to be applied to each element of mset
- @mset@ is the MSet to be mapped
-}
mapMSet :: Eq a => (t -> a) -> MSet t -> MSet a
mapMSet f (MS lst) = MS (combineMultiplicities $ map (\(x, y) -> (f x, y)) lst)
  where
    combineMultiplicities = foldr combineOrInsert []
    combineOrInsert (v, n) acc =
      case lookup v acc of
        Just m  -> (v, n + m) : filter (\(x, _) -> x /= v) acc
        Nothing -> (v, n) : acc