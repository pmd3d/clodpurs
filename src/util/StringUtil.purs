module StringUtil where

import Prelude

import Data.String.CodeUnits as CU
import Data.String.Pattern (Pattern(..))

drop :: Int -> String -> String
drop n s = CU.drop n s

chopSuffix :: Int -> String -> String
chopSuffix n s = CU.take (CU.length s - n) s

chopSuffix1 :: String -> String
chopSuffix1 = chopSuffix 1

ofList :: Array Char -> String
ofList = CU.fromCharArray

isAlnum :: Char -> Boolean
isAlnum c =
  CU.contains (Pattern (CU.singleton c))
    "abcdefghijklmnopqrstuvwxzyABCDEFGHIJKLMNOPQRSTUVWXZY0123456789"
