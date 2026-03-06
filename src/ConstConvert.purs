module ConstConvert where

import Prelude hiding (zero)

import Const as C
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Types as T

pow2_8 :: BigInt
pow2_8 = BigInt.shl (BigInt.fromInt 1) 8.0

pow2_31 :: BigInt
pow2_31 = BigInt.shl (BigInt.fromInt 1) 31.0

pow2_32 :: BigInt
pow2_32 = BigInt.shl (BigInt.fromInt 1) 32.0

pow2_63 :: BigInt
pow2_63 = BigInt.shl (BigInt.fromInt 1) 63.0

pow2_64 :: BigInt
pow2_64 = BigInt.shl (BigInt.fromInt 1) 64.0

half_8 :: BigInt
half_8 = BigInt.fromInt 128

zero :: BigInt
zero = BigInt.fromInt 0

bigIntToInt :: BigInt -> Int
bigIntToInt b = fromMaybe 0 (BigInt.toInt b)

fromNumberSafe :: Number -> BigInt
fromNumberSafe n = fromMaybe zero (BigInt.fromNumber n)

signedWrap :: BigInt -> BigInt -> BigInt -> BigInt
signedWrap v modulus halfMod =
  let u = v `mod` modulus
  in if u >= halfMod then u - modulus else u

constToInt64 :: C.ConstValue -> BigInt
constToInt64 (C.ConstChar c) = BigInt.fromInt c
constToInt64 (C.ConstUChar uc) = BigInt.fromInt uc
constToInt64 (C.ConstInt i) = BigInt.fromInt i
constToInt64 (C.ConstUInt ui) = ui
constToInt64 (C.ConstLong l) = l
constToInt64 (C.ConstULong ul) =
  if ul >= pow2_63 then ul - pow2_64 else ul
constToInt64 (C.ConstDouble d) = fromNumberSafe d

constOfInt64 :: BigInt -> T.CType -> Either String C.ConstValue
constOfInt64 v T.Char = Right (C.ConstChar (bigIntToInt (signedWrap v pow2_8 half_8)))
constOfInt64 v T.SChar = Right (C.ConstChar (bigIntToInt (signedWrap v pow2_8 half_8)))
constOfInt64 v T.UChar = Right (C.ConstUChar (bigIntToInt (v `mod` pow2_8)))
constOfInt64 v T.Int = Right (C.ConstInt (bigIntToInt (signedWrap v pow2_32 pow2_31)))
constOfInt64 v T.Long = Right (C.ConstLong v)
constOfInt64 v T.UInt = Right (C.ConstUInt (v `mod` pow2_32))
constOfInt64 v T.ULong = Right (C.ConstULong (v `mod` pow2_64))
constOfInt64 v (T.Pointer _) = Right (C.ConstULong (v `mod` pow2_64))
constOfInt64 v T.Double = Right (C.ConstDouble (BigInt.toNumber v))
constOfInt64 _ t = Left ("can't convert constant to non_scalar type " <> show t)

uint64ToDouble :: BigInt -> Number
uint64ToDouble = BigInt.toNumber

constConvert :: T.CType -> C.ConstValue -> Either String C.ConstValue
constConvert target_type c =
  if C.typeOfConst c == target_type then Right c
  else case target_type, c of
    T.Double, C.ConstULong ul ->
      Right (C.ConstDouble (uint64ToDouble ul))
    T.Double, _ ->
      Right (C.ConstDouble (BigInt.toNumber (constToInt64 c)))
    T.ULong, C.ConstDouble d ->
      Right (C.ConstULong (fromNumberSafe d `mod` pow2_64))
    _, C.ConstDouble d ->
      constOfInt64 (fromNumberSafe d) target_type
    _, _ ->
      constOfInt64 (constToInt64 c) target_type
