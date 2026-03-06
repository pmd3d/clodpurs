module Types where

import Prelude

import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.List (List(..))

data CType
  = Char
  | SChar
  | UChar
  | Int
  | Long
  | UInt
  | ULong
  | Double
  | Pointer CType
  | Void
  | Array CType BigInt
  | FunType (List CType) CType
  | Structure String

derive instance eqCType :: Eq CType
derive instance ordCType :: Ord CType

instance showCType :: Show CType where
  show Char = "Char"
  show SChar = "SChar"
  show UChar = "UChar"
  show Int = "Int"
  show Long = "Long"
  show UInt = "UInt"
  show ULong = "ULong"
  show Double = "Double"
  show (Pointer inner) = show inner <> "*"
  show Void = "Void"
  show (Array elemType size) =
    "(" <> show elemType <> ", " <> BigInt.toString size <> ")"
  show (FunType paramTypes retType) =
    let paramStr = showParams paramTypes
    in "(FunType (param_types = [" <> paramStr <> "], ret_type = " <> show retType <> "))"
  show (Structure tag) = "(Structure " <> tag <> ")"

showParams :: List CType -> String
showParams l = go l ""
  where
  go Nil _ = ""
  go (Cons x Nil) _ = show x
  go (Cons x rest) _ = show x <> "; " <> go rest ""
