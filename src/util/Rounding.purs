module Rounding where

import Prelude

-- We're assuming that n > 0
roundAwayFromZero :: Int -> Int -> Int
roundAwayFromZero n x
  | x `mod` n == 0 = x
  | x < 0 =
      -- when x is negative and n is positive, x mod n will be negative
      x - n - (x `mod` n)
  | otherwise = x + n - (x `mod` n)
