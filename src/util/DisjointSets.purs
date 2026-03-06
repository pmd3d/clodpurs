module DisjointSets where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))

type DisjointSet a = Map a a

init :: forall a. DisjointSet a
init = Map.empty

union :: forall a. Ord a => a -> a -> DisjointSet a -> DisjointSet a
union x y disjSets = Map.insert x y disjSets

find :: forall a. Ord a => a -> DisjointSet a -> a
find x disjSets =
  case Map.lookup x disjSets of
    Just mappedTo -> find mappedTo disjSets
    Nothing -> x

isEmpty :: forall a. DisjointSet a -> Boolean
isEmpty = Map.isEmpty
