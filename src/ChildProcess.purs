module ChildProcess where

import Prelude

import Effect (Effect)

-- | Run a command with arguments. Throws on non-zero exit code.
-- | Stdout and stderr are inherited (printed to console).
foreign import runCommandImpl :: String -> Array String -> Effect Unit

runCommand :: String -> Array String -> Effect Unit
runCommand = runCommandImpl

-- | Run a command with arguments and capture stdout. Throws on non-zero exit code.
foreign import runCommandOutputImpl :: String -> Array String -> Effect String

runCommandOutput :: String -> Array String -> Effect String
runCommandOutput = runCommandOutputImpl
