module Stream where

import Data.List (List(..), take)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

type Stream a = { items :: List a }

next :: forall a. Stream a -> Maybe (Tuple a (Stream a))
next s = case s.items of
  Cons x rest -> Just (Tuple x { items: rest })
  Nil -> Nothing

peek :: forall a. Stream a -> Maybe a
peek s = case s.items of
  Cons x _ -> Just x
  Nil -> Nothing

npeek :: forall a. Int -> Stream a -> List a
npeek n s = take n s.items

isEmpty :: forall a. Stream a -> Boolean
isEmpty s = case s.items of
  Nil -> true
  _ -> false

ofList :: forall a. List a -> Stream a
ofList lst = { items: lst }
