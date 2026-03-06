module TokStream where

import Data.Either (Either(..))
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Stream as Stream
import Tokens (Token)

type TokStream = Stream.Stream Token

takeToken :: TokStream -> Either String (Tuple Token TokStream)
takeToken tokens = case Stream.next tokens of
  Just (Tuple tok rest) -> Right (Tuple tok rest)
  Nothing -> Left "Unexpected end of file"

peek :: TokStream -> Maybe Token
peek = Stream.peek

npeek :: Int -> TokStream -> List Token
npeek = Stream.npeek

isEmpty :: TokStream -> Boolean
isEmpty = Stream.isEmpty

ofList :: List Token -> TokStream
ofList = Stream.ofList
