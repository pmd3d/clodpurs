module AssemblySymbols where

import Prelude

import Assembly (AsmReg, AsmType(..))
import CompilerError (CompilerError(..))
import Data.Either (Either(..))
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import RegSet as RegSet

type AsmFunEntry =
  { defined :: Boolean
  , bytes_required :: Int
  , return_on_stack :: Boolean
  , param_regs :: List AsmReg
  , return_regs :: List AsmReg
  , callee_saved_regs_used :: RegSet.RegSet
  }

type AsmObjEntry =
  { t :: AsmType
  , is_static :: Boolean
  , constant :: Boolean
  }

data AsmSymbolEntry
  = Fun AsmFunEntry
  | Obj AsmObjEntry

derive instance eqAsmSymbolEntry :: Eq AsmSymbolEntry

type AsmSymbolTableMap = Map String AsmSymbolEntry

empty :: AsmSymbolTableMap
empty = Map.empty

addFun :: String -> Boolean -> Boolean -> List AsmReg -> List AsmReg -> AsmSymbolTableMap -> AsmSymbolTableMap
addFun fun_name defined return_on_stack param_regs return_regs ast =
  Map.insert fun_name (Fun
    { defined: defined
    , bytes_required: 0
    , callee_saved_regs_used: RegSet.empty
    , return_on_stack: return_on_stack
    , param_regs: param_regs
    , return_regs: return_regs
    }) ast

addVar :: String -> AsmType -> Boolean -> AsmSymbolTableMap -> AsmSymbolTableMap
addVar var_name t is_static ast =
  Map.insert var_name (Obj { t: t, is_static: is_static, constant: false }) ast

addConstant :: String -> AsmType -> AsmSymbolTableMap -> AsmSymbolTableMap
addConstant const_name t ast =
  Map.insert const_name (Obj { t: t, is_static: true, constant: true }) ast

find :: String -> AsmSymbolTableMap -> Either CompilerError AsmSymbolEntry
find name ast =
  case Map.lookup name ast of
    Just v -> Right v
    Nothing -> Left (InternalError ("assembly symbol not found: " <> name))

setBytesRequired :: String -> Int -> AsmSymbolTableMap -> Either CompilerError AsmSymbolTableMap
setBytesRequired fun_name bytes_required ast =
  case find fun_name ast of
    Right (Fun f) -> Right (Map.insert fun_name (Fun (f { bytes_required = bytes_required })) ast)
    Right (Obj _) -> Left (InternalError "not a function")
    Left e -> Left e

getBytesRequired :: String -> AsmSymbolTableMap -> Either CompilerError Int
getBytesRequired fun_name ast =
  case find fun_name ast of
    Right (Fun f) -> Right f.bytes_required
    Right (Obj _) -> Left (InternalError "not a function")
    Left e -> Left e

addCalleeSavedRegsUsed :: String -> RegSet.RegSet -> AsmSymbolTableMap -> Either CompilerError AsmSymbolTableMap
addCalleeSavedRegsUsed fun_name regs ast =
  case find fun_name ast of
    Right (Fun f) ->
      Right (Map.insert fun_name (Fun (f { callee_saved_regs_used = RegSet.union f.callee_saved_regs_used regs })) ast)
    Right (Obj _) -> Left (InternalError "not a function")
    Left e -> Left e

getCalleeSavedRegsUsed :: String -> AsmSymbolTableMap -> Either CompilerError RegSet.RegSet
getCalleeSavedRegsUsed fun_name ast =
  case find fun_name ast of
    Right (Fun f) -> Right f.callee_saved_regs_used
    Right (Obj _) -> Left (InternalError "not a function")
    Left e -> Left e

getSize :: String -> AsmSymbolTableMap -> Either CompilerError Int
getSize var_name ast =
  case find var_name ast of
    Right (Obj { t: Byte }) -> Right 1
    Right (Obj { t: Longword }) -> Right 4
    Right (Obj { t: Quadword }) -> Right 8
    Right (Obj { t: AsmDouble }) -> Right 8
    Right (Obj { t: ByteArray { size: size } }) -> Right size
    Right (Fun _) -> Left (InternalError "this is a function, not an object")
    Left e -> Left e

getType :: String -> AsmSymbolTableMap -> Either CompilerError AsmType
getType var_name ast =
  case find var_name ast of
    Right (Obj { t: t }) -> Right t
    Right (Fun _) -> Left (InternalError "this is a function, not an object")
    Left e -> Left e

getAlignment :: String -> AsmSymbolTableMap -> Either CompilerError Int
getAlignment var_name ast =
  case find var_name ast of
    Right (Obj { t: Byte }) -> Right 1
    Right (Obj { t: Longword }) -> Right 4
    Right (Obj { t: Quadword }) -> Right 8
    Right (Obj { t: AsmDouble }) -> Right 8
    Right (Obj { t: ByteArray { alignment: alignment } }) -> Right alignment
    Right (Fun _) -> Left (InternalError "this is a function, not an object")
    Left e -> Left e

isDefined :: String -> AsmSymbolTableMap -> Either CompilerError Boolean
isDefined fun_name ast =
  case find fun_name ast of
    Right (Fun { defined: defined }) -> Right defined
    Right _ -> Left (InternalError "not a function")
    Left e -> Left e

isStatic :: String -> AsmSymbolTableMap -> Either CompilerError Boolean
isStatic var_name ast =
  case find var_name ast of
    Right (Obj o) -> Right o.is_static
    Right (Fun _) -> Left (InternalError "functions don't have storage duration")
    Left e -> Left e

isConstant :: String -> AsmSymbolTableMap -> Either CompilerError Boolean
isConstant name ast =
  case find name ast of
    Right (Obj { constant: true }) -> Right true
    Right (Obj _) -> Right false
    Right (Fun _) -> Left (InternalError "is_constant doesn't make sense for functions")
    Left e -> Left e

returnsOnStack :: String -> AsmSymbolTableMap -> Either CompilerError Boolean
returnsOnStack fun_name ast =
  case find fun_name ast of
    Right (Fun f) -> Right f.return_on_stack
    Right (Obj _) -> Left (InternalError "this is an object, not a function")
    Left e -> Left e

paramRegsUsed :: String -> AsmSymbolTableMap -> Either CompilerError (List AsmReg)
paramRegsUsed fun_name ast =
  case find fun_name ast of
    Right (Fun f) -> Right f.param_regs
    Right (Obj _) -> Left (InternalError "not a function")
    Left e -> Left e

returnRegsUsed :: String -> AsmSymbolTableMap -> Either CompilerError (List AsmReg)
returnRegsUsed fun_name ast =
  case find fun_name ast of
    Right (Fun f) -> Right f.return_regs
    Right (Obj _) -> Left (InternalError "not a function")
    Left e -> Left e
