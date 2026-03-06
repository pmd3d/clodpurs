module Symbols where

import Prelude

import CompilerError (CompilerError(..))
import Data.BigInt as BigInt
import Data.Either (Either(..))
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Initializers (StaticInit(..))
import Types (CType(..))
import UniqueIds as UniqueIds

data InitialValue
  = Tentative
  | Initial (List StaticInit)
  | NoInitializer

derive instance eqInitialValue :: Eq InitialValue
derive instance ordInitialValue :: Ord InitialValue

type FunAttr = { defined :: Boolean, isGlobal :: Boolean }
type StaticAttr = { init :: InitialValue, isGlobal :: Boolean }

data IdentifierAttrs
  = FunAttr FunAttr
  | StaticAttr StaticAttr
  | ConstAttr StaticInit
  | LocalAttr

derive instance eqIdentifierAttrs :: Eq IdentifierAttrs
derive instance ordIdentifierAttrs :: Ord IdentifierAttrs

type SymbolEntry = { symType :: CType, attrs :: IdentifierAttrs }

type SymbolTableMap = Map String SymbolEntry

empty :: SymbolTableMap
empty = Map.empty

-- always use replace instead of add; we want to remove old binding when we add a new one

addAutomaticVar :: String -> CType -> SymbolTableMap -> SymbolTableMap
addAutomaticVar name t st =
  Map.insert name { symType: t, attrs: LocalAttr } st

addStaticVar :: String -> CType -> Boolean -> InitialValue -> SymbolTableMap -> SymbolTableMap
addStaticVar name t isGlobal_ init st =
  Map.insert name { symType: t, attrs: StaticAttr { init: init, isGlobal: isGlobal_ } } st

addFun :: String -> CType -> Boolean -> Boolean -> SymbolTableMap -> SymbolTableMap
addFun name t isGlobal_ defined st =
  Map.insert name { symType: t, attrs: FunAttr { isGlobal: isGlobal_, defined: defined } } st

get :: String -> SymbolTableMap -> Either CompilerError SymbolEntry
get name st =
  case Map.lookup name st of
    Just v -> Right v
    Nothing -> Left (InternalError ("symbol not found: " <> name))

getOpt :: String -> SymbolTableMap -> Maybe SymbolEntry
getOpt name st = Map.lookup name st

addStringWithCounter :: UniqueIds.Counter -> String -> SymbolTableMap -> Tuple UniqueIds.Counter (Tuple String SymbolTableMap)
addStringWithCounter counter s st =
  let (Tuple counter' str_id) = UniqueIds.makeNamedTemporary "string" counter
      t = Array Char (BigInt.fromInt (String.length s + 1))
      st' = Map.insert str_id { symType: t, attrs: ConstAttr (StringInit s true) } st
  in Tuple counter' (Tuple str_id st')

isGlobal :: String -> SymbolTableMap -> Either CompilerError Boolean
isGlobal name st =
  map
    ( \entry -> case entry.attrs of
        LocalAttr -> false
        ConstAttr _ -> false
        StaticAttr r -> r.isGlobal
        FunAttr r -> r.isGlobal
    )
    (get name st)

bindings :: SymbolTableMap -> List (Tuple String SymbolEntry)
bindings st = Map.toUnfoldable st
