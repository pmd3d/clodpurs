module UniqueIds where

import Prelude

import Data.Tuple (Tuple(..))

type Counter = Int

initialCounter :: Counter
initialCounter = 0

makeTemporary :: Counter -> Tuple Counter String
makeTemporary counter =
  Tuple (counter + 1) ("tmp." <> show counter)

makeLabel :: String -> Counter -> Tuple Counter String
makeLabel prefix counter =
  Tuple (counter + 1) (prefix <> "." <> show counter)

makeNamedTemporary :: String -> Counter -> Tuple Counter String
makeNamedTemporary = makeLabel
