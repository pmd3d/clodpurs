module Initializers where

import Prelude

import CompilerError (CompilerError)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either)
import Data.List (List(..))
import Data.Maybe (fromMaybe)
import TypeTable (TypeTableMap)
import TypeUtils as TypeUtils
import Types (CType)

data StaticInit
  = CharInit Int
  | UCharInit Int
  | IntInit Int
  | LongInit BigInt
  | UIntInit BigInt
  | ULongInit BigInt
  | DoubleInit Number
  | ZeroInit Int
  | StringInit String Boolean -- flag indicates whether the string is null terminated
  | PointerInit String -- pointer to static variable

derive instance eqStaticInit :: Eq StaticInit
derive instance ordStaticInit :: Ord StaticInit

instance showStaticInit :: Show StaticInit where
  show (CharInit c) = show c
  show (UCharInit uc) = show uc
  show (IntInit i) = show i
  show (LongInit l) = BigInt.toString l <> "l"
  show (UIntInit u) = BigInt.toString u <> "u"
  show (ULongInit ul) = BigInt.toString ul <> "ul"
  show (DoubleInit dbl) = show dbl
  show (ZeroInit i) = "zero[" <> show i <> "]"
  show (StringInit s b) = "\"" <> s <> (if b then "\\0" else "") <> "\""
  show (PointerInit s) = "&" <> s

zero :: TypeTableMap -> CType -> Either CompilerError (List StaticInit)
zero tt t = map (\size -> Cons (ZeroInit (fromMaybe 0 (BigInt.toInt size))) Nil) (TypeUtils.getSize tt t)

isZero :: StaticInit -> Boolean
isZero = case _ of
  CharInit c -> c == 0
  IntInit i -> i == 0
  LongInit l -> l == BigInt.fromInt 0
  UCharInit c -> c == 0
  UIntInit u -> u == BigInt.fromInt 0
  ULongInit ul -> ul == BigInt.fromInt 0
  -- NOTE: consider all doubles non-zero since we don't know if it's zero or negative zero
  DoubleInit _ -> false
  ZeroInit _ -> true
  PointerInit _ -> false
  StringInit _ _ -> false
