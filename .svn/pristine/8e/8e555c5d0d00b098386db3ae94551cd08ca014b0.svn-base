module Alloy.Relation where

import Data.Set as Set

type Tuple a = [a]
type Relation a = Set (Tuple a)

arity :: Ord a => Relation a -> Maybe Int
arity r = case Set.elems (Set.map length r) of [n] -> Just n
                                               _   -> Nothing

project :: Ord a => Relation a -> Int -> [a]
project r n = Set.toList $ Set.map (!!(n-1)) r

none :: Relation a
none = Set.empty

null :: Relation a -> Bool
null = Set.null

singleton :: Tuple a -> Relation a
singleton t = Set.singleton t

fromList :: Ord a => [[a]] -> Relation a
fromList = Set.fromList

toList :: Ord a => Relation a -> [[a]]
toList = Set.toList

arrow :: Ord a => Relation a -> Relation a -> Relation a
arrow r s = Set.fromList [t++u | t <- Set.toList r, u <- Set.toList s]

union :: Ord a => Relation a -> Relation a -> Relation a
union = Set.union

intersection :: Ord a => Relation a -> Relation a -> Relation a
intersection = Set.intersection

converse :: Ord a => Relation a -> Relation a
converse = Set.map reverse

join :: Ord a => Relation a -> Relation a -> Relation a
join r s = Set.fromList [init t ++ tail u | t <- Set.toList r, u <- Set.toList s, last t == head u]

transclosure :: Ord a => Relation a -> Relation a
transclosure r = aux r r 
    where aux r a = if join r a `isSubsetOf` a then a else transclosure (a `Set.union` join r a)

ranrestriction :: Ord a => Relation a -> Relation a -> Relation a
ranrestriction r s = Set.fromList [t | t <- Set.toList r, Set.member [last t] s]

domrestriction :: Ord a => Relation a -> Relation a -> Relation a
domrestriction r s = Set.fromList [t | t <- Set.toList s, Set.member [head t] r]