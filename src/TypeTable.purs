module TypeTable where

import Prelude

import CompilerError (CompilerError(..))
import Data.Either (Either(..))
import Data.List (List, sortBy)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import Types (CType)

type MemberDef = { memberType :: CType, offset :: Int }

type StructDef = { alignment :: Int, size :: Int, members :: Map String MemberDef }

type TypeTableMap = Map String StructDef

empty :: TypeTableMap
empty = Map.empty

addStructDefinition :: String -> StructDef -> TypeTableMap -> TypeTableMap
addStructDefinition tag structDef tt = Map.insert tag structDef tt

mem :: String -> TypeTableMap -> Boolean
mem tag tt = Map.member tag tt

tryFind :: String -> TypeTableMap -> Maybe StructDef
tryFind tag tt = Map.lookup tag tt

find :: String -> TypeTableMap -> Either CompilerError StructDef
find tag tt =
  case Map.lookup tag tt of
    Just v -> Right v
    Nothing -> Left (InternalError ("type table missing tag " <> tag))

getMembers :: String -> TypeTableMap -> Either CompilerError (List MemberDef)
getMembers tag tt =
  map
    ( \structDef ->
        let compareOffset m1 m2 = compare m1.offset m2.offset
        in sortBy compareOffset (map snd (Map.toUnfoldable structDef.members :: List _))
    )
    (find tag tt)

getMemberTypes :: String -> TypeTableMap -> Either CompilerError (List CType)
getMemberTypes tag tt =
  map (map _.memberType) (getMembers tag tt)
