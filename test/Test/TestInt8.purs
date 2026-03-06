module Test.TestInt8 where

import Prelude

import Data.BigInt (fromInt, fromString)
import Data.Maybe (fromJust)
import Int8 (ofInt, ofInt64)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

unsafeBigInt :: String -> _
unsafeBigInt s = unsafePartial (fromJust (fromString s))

spec :: Spec Unit
spec = describe "Int8" do
  it "int to int8" do
    show (ofInt 100) `shouldEqual` "100"

  it "wrapped int to int8" do
    show (ofInt 128) `shouldEqual` "-128"

  it "int64 to int8" do
    show (ofInt64 (fromInt (-110))) `shouldEqual` "-110"

  it "wrapped int64 to int8" do
    show (ofInt64 (unsafeBigInt "1239235325")) `shouldEqual` "-3"

  it "compare int8 values" do
    let twelve = ofInt 268
    let fourteen = ofInt (-4082)
    compare twelve fourteen `shouldEqual` LT
