module Bytes where

import Prelude

import Data.Char (toCharCode)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Int.Bits ((.&.), (.|.), shl)
import Data.Maybe (Maybe(..))

-- Byte array represented as an Array of Int (each 0-255)
type Bytes = Array Int

foreign import ofStringImpl :: String -> Array Int

ofString :: String -> Bytes
ofString = ofStringImpl

foreign import makeImpl :: Int -> Int -> Array Int

make :: Int -> Char -> Bytes
make n c = makeImpl n (toCharCode c)

length :: Bytes -> Int
length bs = arrayLength bs

foreign import arrayLength :: Array Int -> Int

cat :: Bytes -> Bytes -> Bytes
cat = appendArrays

foreign import appendArrays :: Array Int -> Array Int -> Array Int

foreign import sliceArray :: Int -> Int -> Array Int -> Array Int

sub :: Bytes -> Int -> Int -> Bytes
sub b offset len = sliceArray offset (offset + len) b

foreign import unsafeIndex :: Array Int -> Int -> Int

foreign import getInt64LeImpl :: Array Int -> Int -> String

-- Read a little-endian Int64 as BigInt from 8 bytes at offset
getInt64Le :: Bytes -> Int -> BigInt
getInt64Le b offset =
  case BigInt.fromString (getInt64LeImpl b offset) of
    Nothing -> BigInt.fromInt 0
    Just n -> n

-- Read a little-endian Int32 as Int from 4 bytes at offset
getInt32Le :: Bytes -> Int -> Int
getInt32Le b offset =
  let byte i = unsafeIndex b (offset + i)
  in byte 0 .|. (byte 1 `shl` 8) .|. (byte 2 `shl` 16) .|. (byte 3 `shl` 24)

-- Read a signed byte (Int8) as Int
getInt8 :: Bytes -> Int -> Int
getInt8 b offset =
  let v = unsafeIndex b offset
  in if v >= 128 then v - 256 else v
