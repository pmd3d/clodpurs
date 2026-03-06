module Int8 where

import Prelude

import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Int.Bits ((.&.), (.|.))
import Data.Maybe (fromMaybe)

-- Represent signed chars internally with the Int type (32-bit signed)
type Int8Value = Int

zero :: Int8Value
zero = 0

-- Internal function to sign-or-zero-extend into upper bytes
resetUpperBytes :: Int8Value -> Int8Value
resetUpperBytes x =
  if x .&. 128 == 0 then
    -- result is positive, zero out upper bits
    x .&. 0x000000ff
  else
    -- result is negative, set upper bits to 1
    x .|. (-256) -- 0xffffff00 as signed int

ofInt :: Int -> Int8Value
ofInt = resetUpperBytes

toInt :: Int8Value -> Int
toInt x = x

ofInt64 :: BigInt -> Int8Value
ofInt64 i = resetUpperBytes (fromMaybe 0 (BigInt.toInt i))

toInt64 :: Int8Value -> BigInt
toInt64 = BigInt.fromInt

toString :: Int8Value -> String
toString = show
