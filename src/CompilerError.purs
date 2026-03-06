module CompilerError where

import Prelude

data CompilerError
  = LexError String
  | ParseError String
  | ResolveError String
  | LoopLabelError String
  | TypeError String
  | DriverError String
  | InternalError String

instance showCompilerError :: Show CompilerError where
  show (LexError msg) = "Lex error: " <> msg
  show (ParseError msg) = "Parse error: " <> msg
  show (ResolveError msg) = "Resolve error: " <> msg
  show (LoopLabelError msg) = "Loop label error: " <> msg
  show (TypeError msg) = "Type error: " <> msg
  show (DriverError msg) = "Driver error: " <> msg
  show (InternalError msg) = "Internal error: " <> msg
