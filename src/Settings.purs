module Settings where

import Prelude

data Stage
  = Lex
  | Parse
  | Validate
  | Tacky
  | Codegen
  | Assembly
  | Obj
  | Executable

derive instance eqStage :: Eq Stage
derive instance ordStage :: Ord Stage
instance showStage :: Show Stage where
  show Lex = "Lex"
  show Parse = "Parse"
  show Validate = "Validate"
  show Tacky = "Tacky"
  show Codegen = "Codegen"
  show Assembly = "Assembly"
  show Obj = "Obj"
  show Executable = "Executable"

data Target = OS_X | Linux

derive instance eqTarget :: Eq Target
derive instance ordTarget :: Ord Target
instance showTarget :: Show Target where
  show OS_X = "OS_X"
  show Linux = "Linux"

type CompilerConfig =
  { platform :: Target
  , debug :: Boolean
  }

type Optimizations =
  { constant_folding :: Boolean
  , dead_store_elimination :: Boolean
  , unreachable_code_elimination :: Boolean
  , copy_propagation :: Boolean
  }
