module ResultUtil where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..), reverse, zip)
import Data.Tuple (Tuple(..))

resultTraverse :: forall a b e. (a -> Either e b) -> List a -> Either e (List b)
resultTraverse f items =
  map reverse (go items (Right Nil))
  where
  go Nil acc = acc
  go (Cons item rest) acc = case acc of
    Left e -> Left e
    Right xs -> case f item of
      Left e -> Left e
      Right x -> go rest (Right (Cons x xs))

resultFold :: forall a s e. (s -> a -> Either e s) -> s -> List a -> Either e s
resultFold f state items =
  go items (Right state)
  where
  go Nil acc = acc
  go (Cons item rest) acc = case acc of
    Left e -> Left e
    Right s -> go rest (f s item)

resultTraverse2 :: forall a b c e. (a -> b -> Either e c) -> List a -> List b -> Either e (List c)
resultTraverse2 f xs ys =
  map reverse (go (zip xs ys) (Right Nil))
  where
  go Nil acc = acc
  go (Cons (Tuple x y) rest) acc = case acc of
    Left e -> Left e
    Right zs -> case f x y of
      Left e -> Left e
      Right z -> go rest (Right (Cons z zs))

resultFold2 :: forall a b s e. (s -> a -> b -> Either e s) -> s -> List a -> List b -> Either e s
resultFold2 f state xs ys =
  go (zip xs ys) (Right state)
  where
  go Nil acc = acc
  go (Cons (Tuple x y) rest) acc = case acc of
    Left e -> Left e
    Right s -> go rest (f s x y)
