module Optimize where

import Settings (Optimizations)
import Tacky (TackyProgram)

optimize :: Optimizations -> String -> TackyProgram -> TackyProgram
optimize _ _ tackyProgram = tackyProgram
