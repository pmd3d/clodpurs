module AddressTaken where

import Prelude

import Data.Foldable (foldl)
import Data.List (List, mapMaybe)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Tacky (TackyInstruction(..), TackyProgram(..), TackyTopLevel(..), TackyVal(..))

analyze :: List TackyInstruction -> Set String
analyze instrs =
  Set.fromFoldable (mapMaybe addrTaken instrs)
  where
  addrTaken (GetAddress { src: Var v }) = Just v
  addrTaken _ = Nothing

analyzeProgram :: TackyProgram -> Set String
analyzeProgram (TProgram tls) =
  let analyzeTl (TFunction f) = analyze f.body
      analyzeTl _ = Set.empty
      aliasedVarsPerFun = map analyzeTl tls
  in foldl Set.union Set.empty aliasedVarsPerFun
