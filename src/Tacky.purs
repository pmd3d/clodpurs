module Tacky where

import Prelude

import CompilerError (CompilerError)
import Const as Const
import Data.Either (Either)
import Data.List (List)
import Data.Maybe (Maybe)
import Initializers (StaticInit)
import Symbols as Symbols
import Types as Types

data TackyUnaryOperator = Complement | Negate | Not

derive instance eqTackyUnaryOperator :: Eq TackyUnaryOperator
derive instance ordTackyUnaryOperator :: Ord TackyUnaryOperator

data TackyBinaryOperator
  = Add
  | Subtract
  | Multiply
  | Divide
  | Mod
  | Equal
  | NotEqual
  | LessThan
  | LessOrEqual
  | GreaterThan
  | GreaterOrEqual

derive instance eqTackyBinaryOperator :: Eq TackyBinaryOperator
derive instance ordTackyBinaryOperator :: Ord TackyBinaryOperator

data TackyVal
  = Constant Const.ConstValue
  | Var String

derive instance eqTackyVal :: Eq TackyVal
derive instance ordTackyVal :: Ord TackyVal

showTackyVal :: TackyVal -> String
showTackyVal (Constant c) = show c
showTackyVal (Var v) = v

typeOfVal :: Symbols.SymbolTableMap -> TackyVal -> Either CompilerError Types.CType
typeOfVal _ (Constant c) = pure (Const.typeOfConst c)
typeOfVal st (Var v) = map _.symType (Symbols.get v st)

type TackySrcDst = { src :: TackyVal, dst :: TackyVal }

type TackyUnaryInfo = { op :: TackyUnaryOperator, src :: TackyVal, dst :: TackyVal }

type TackyBinaryInfo =
  { op :: TackyBinaryOperator
  , src1 :: TackyVal
  , src2 :: TackyVal
  , dst :: TackyVal
  }

type TackyAddPtrInfo =
  { ptr :: TackyVal
  , index :: TackyVal
  , scale :: Int
  , dst :: TackyVal
  }

type TackyCopyToOffsetInfo = { src :: TackyVal, dst :: String, offset :: Int }

type TackyCopyFromOffsetInfo = { src :: String, offset :: Int, dst :: TackyVal }

type TackyFunCallInfo = { f :: String, args :: List TackyVal, dst :: Maybe TackyVal }

type TackyLoadInfo = { src_ptr :: TackyVal, dst :: TackyVal }
type TackyStoreInfo = { src :: TackyVal, dst_ptr :: TackyVal }

data TackyInstruction
  = Return (Maybe TackyVal)
  | SignExtend TackySrcDst
  | ZeroExtend TackySrcDst
  | DoubleToInt TackySrcDst
  | IntToDouble TackySrcDst
  | DoubleToUInt TackySrcDst
  | UIntToDouble TackySrcDst
  | Truncate TackySrcDst
  | TUnary TackyUnaryInfo
  | TBinary TackyBinaryInfo
  | Copy TackySrcDst
  | GetAddress TackySrcDst
  | Load TackyLoadInfo
  | Store TackyStoreInfo
  | AddPtr TackyAddPtrInfo
  | CopyToOffset TackyCopyToOffsetInfo
  | CopyFromOffset TackyCopyFromOffsetInfo
  | Jump String
  | JumpIfZero TackyVal String
  | JumpIfNotZero TackyVal String
  | TLabel String
  | FunCall TackyFunCallInfo

derive instance eqTackyInstruction :: Eq TackyInstruction
derive instance ordTackyInstruction :: Ord TackyInstruction

type TackyFunctionDef =
  { name :: String
  , isGlobal :: Boolean
  , paramList :: List String
  , body :: List TackyInstruction
  }

type TackyStaticVariableDef =
  { name :: String
  , t :: Types.CType
  , isGlobal :: Boolean
  , init :: List StaticInit
  }

type TackyStaticConstantDef =
  { name :: String
  , t :: Types.CType
  , init :: StaticInit
  }

data TackyTopLevel
  = TFunction TackyFunctionDef
  | TStaticVariable TackyStaticVariableDef
  | TStaticConstant TackyStaticConstantDef

derive instance eqTackyTopLevel :: Eq TackyTopLevel

data TackyProgram = TProgram (List TackyTopLevel)

derive instance eqTackyProgram :: Eq TackyProgram
