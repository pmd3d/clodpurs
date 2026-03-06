module RegSet where

import Assembly (AsmReg)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe)
import Data.Foldable (foldl) as Foldable
import Data.Set (Set)
import Data.Set as Set

type RegSet = Set AsmReg

empty :: RegSet
empty = Set.empty

singleton :: AsmReg -> RegSet
singleton = Set.singleton

add :: AsmReg -> RegSet -> RegSet
add = Set.insert

remove :: AsmReg -> RegSet -> RegSet
remove = Set.delete

mem :: AsmReg -> RegSet -> Boolean
mem = Set.member

union :: RegSet -> RegSet -> RegSet
union = Set.union

inter :: RegSet -> RegSet -> RegSet
inter = Set.intersection

diff :: RegSet -> RegSet -> RegSet
diff = Set.difference

isEmpty :: RegSet -> Boolean
isEmpty = Set.isEmpty

cardinal :: RegSet -> Int
cardinal = Set.size

fold :: forall a. (a -> AsmReg -> a) -> RegSet -> a -> a
fold f s acc = Foldable.foldl f acc s

map :: (AsmReg -> AsmReg) -> RegSet -> RegSet
map = Set.map

filter :: (AsmReg -> Boolean) -> RegSet -> RegSet
filter = Set.filter

exists :: (AsmReg -> Boolean) -> RegSet -> Boolean
exists f s = List.any f (elements s)

forAll :: (AsmReg -> Boolean) -> RegSet -> Boolean
forAll f s = List.all f (elements s)

elements :: RegSet -> List AsmReg
elements = Set.toUnfoldable

ofList :: List AsmReg -> RegSet
ofList = Set.fromFoldable

subset :: RegSet -> RegSet -> Boolean
subset = Set.subset

choose :: RegSet -> Maybe AsmReg
choose = Set.findMin
