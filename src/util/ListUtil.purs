module ListUtil where

import Prelude

import Data.List (List(..), (:), sortBy, reverse, head, last)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

tryMax :: forall a. (a -> a -> Ordering) -> List a -> Maybe a
tryMax _ Nil = Nothing
tryMax cmp l = head (reverse (sortBy cmp l))

tryMin :: forall a. (a -> a -> Ordering) -> List a -> Maybe a
tryMin _ Nil = Nothing
tryMin cmp l = head (sortBy cmp l)

makeList :: forall a. Int -> a -> List a
makeList n v
  | n <= 0 = Nil
  | otherwise = v : makeList (n - 1) v

tryLast :: forall a. List a -> Maybe a
tryLast = last

take :: forall a. Int -> List a -> List a
take _ Nil = Nil
take n _ | n <= 0 = Nil
take n (h : t) = h : take (n - 1) t

takeDrop :: forall a. Int -> List a -> Tuple (List a) (List a)
takeDrop n (h : t) | n > 0 =
  let Tuple l1 l2 = takeDrop (n - 1) t
  in Tuple (h : l1) l2
takeDrop _ l = Tuple Nil l
