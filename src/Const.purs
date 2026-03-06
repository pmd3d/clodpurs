module Const where

import Prelude

import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Types as Types

data ConstValue
  = ConstChar Int
  | ConstUChar Int
  | ConstInt Int
  | ConstLong BigInt
  | ConstUInt BigInt
  | ConstULong BigInt
  | ConstDouble Number

derive instance eqConstValue :: Eq ConstValue
derive instance ordConstValue :: Ord ConstValue

instance showConstValue :: Show ConstValue where
  show (ConstChar c) = "ConstChar " <> show c
  show (ConstUChar c) = "ConstUChar " <> show c
  show (ConstInt i) = "ConstInt " <> show i
  show (ConstLong l) = "ConstLong " <> BigInt.toString l <> "L"
  show (ConstUInt u) = "ConstUInt " <> BigInt.toString u <> "U"
  show (ConstULong ul) = "ConstULong " <> BigInt.toString ul <> "UL"
  show (ConstDouble d) = "ConstDouble " <> show d

intZero :: ConstValue
intZero = ConstInt 0

intOne :: ConstValue
intOne = ConstInt 1

typeOfConst :: ConstValue -> Types.CType
typeOfConst (ConstChar _) = Types.SChar
typeOfConst (ConstUChar _) = Types.UChar
typeOfConst (ConstInt _) = Types.Int
typeOfConst (ConstLong _) = Types.Long
typeOfConst (ConstUInt _) = Types.UInt
typeOfConst (ConstULong _) = Types.ULong
typeOfConst (ConstDouble _) = Types.Double
