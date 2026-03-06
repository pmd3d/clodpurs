module Main where

import Prelude

import ChildProcess (runCommand)
import Compile as Compile
import CompilerError (CompilerError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (error)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, readTextFile, unlink, writeTextFile)
import Node.Path (extname)
import Node.Process (argv, exit', platformStr)
import Settings (CompilerConfig, Optimizations, Stage(..), Target(..))

-- | Detect current platform from Node.js process.platform
currentPlatform :: Target
currentPlatform =
  if platformStr == "darwin" then OS_X
  else Linux

-- | Validate that a file has .c or .h extension
validateExtension :: String -> Effect Unit
validateExtension filename =
  let ext = extname filename
  in when (ext /= ".c" && ext /= ".h") do
    error "Expected C source file with .c or .h extension"
    exit' 1

-- | Preprocess a C source file using gcc -E -P
preprocess :: String -> Effect String
preprocess src = do
  validateExtension src
  let output = Compile.changeExtension ".i" src
  runCommand "gcc" ["-E", "-P", src, "-o", output]
  pure output

-- | Write debug files, graphviz DOTs, and assembly from a CompileResult
writeCompileResult :: Compile.CompileResult -> Effect Unit
writeCompileResult result = do
  -- Write debug files
  for_ result.debugFiles \(Tuple path content) ->
    writeTextFile UTF8 path content
  -- Write graphviz DOT files and generate PNGs
  for_ result.graphvizDots \(Tuple label dotContent) -> do
    let dotFile = label <> ".dot"
    let pngFile = Compile.changeExtension ".png" dotFile
    writeTextFile UTF8 dotFile dotContent
    runCommand "dot" ["-Tpng", dotFile, "-o", pngFile]
  -- Write assembly file
  case result.assemblyFile of
    Just (Tuple path content) -> writeTextFile UTF8 path content
    Nothing -> pure unit

-- | Compile a preprocessed source file. Returns the .s path on success, exits on error.
compile' :: CompilerConfig -> Stage -> Optimizations -> String -> Effect String
compile' config stage optimizations preprocessedSrc = do
  source <- readTextFile UTF8 preprocessedSrc
  case Compile.compile config stage optimizations preprocessedSrc source of
    Right result -> do
      writeCompileResult result
      unlink preprocessedSrc
      pure (Compile.changeExtension ".s" preprocessedSrc)
    Left err -> do
      unlink preprocessedSrc
      error (show (err :: CompilerError))
      exit' 1

-- | Assemble and/or link an assembly file
assembleAndLink :: Boolean -> Boolean -> Array String -> String -> Effect Unit
assembleAndLink link cleanup libs src = do
  let linkOption = if link then [] else ["-c"]
  let libOptions = map (\l -> "-l" <> l) libs
  let assemblyFile = Compile.changeExtension ".s" src
  let outputFile =
        if link then Compile.changeExtension "" src
        else Compile.changeExtension ".o" src
  runCommand "gcc" (linkOption <> [assemblyFile] <> libOptions <> ["-o", outputFile])
  when cleanup (unlink assemblyFile)

-- | Main driver: preprocess -> compile -> assemble/link
driver :: Target -> Boolean -> Array String -> Stage -> Optimizations -> String -> Effect Unit
driver target debug libs stage optimizations src = do
  let config = { platform: target, debug: debug }
  preprocessedName <- preprocess src
  assemblyName <- compile' config stage optimizations preprocessedName
  case stage of
    Executable ->
      assembleAndLink true (not debug) libs assemblyName
    Obj ->
      assembleAndLink false (not debug) [] assemblyName
    _ -> pure unit

-- | Set optimization flags: if --optimize, enable all; otherwise use individual flags
setOptions :: Boolean -> Boolean -> Boolean -> Boolean -> Boolean -> Optimizations
setOptions optimize constantFolding deadStoreElimination copyPropagation unreachableCodeElimination =
  if optimize then
    { constant_folding: true
    , dead_store_elimination: true
    , unreachable_code_elimination: true
    , copy_propagation: true
    }
  else
    { constant_folding: constantFolding
    , dead_store_elimination: deadStoreElimination
    , copy_propagation: copyPropagation
    , unreachable_code_elimination: unreachableCodeElimination
    }

-- | CLI argument parsing state
type ParseState =
  { stage :: Stage
  , target :: Target
  , debug :: Boolean
  , libs :: Array String
  , optimize :: Boolean
  , foldConstants :: Boolean
  , eliminateDeadStores :: Boolean
  , propagateCopies :: Boolean
  , eliminateUnreachableCode :: Boolean
  , srcFile :: Maybe String
  }

defaultState :: ParseState
defaultState =
  { stage: Executable
  , target: currentPlatform
  , debug: false
  , libs: []
  , optimize: false
  , foldConstants: false
  , eliminateDeadStores: false
  , propagateCopies: false
  , eliminateUnreachableCode: false
  , srcFile: Nothing
  }

-- | Parse command-line arguments manually (matching F# driver options)
parseArgs :: Array String -> Effect ParseState
parseArgs argv = do
  let userArgs = Array.drop 2 argv -- drop node path + script path
  let expanded = expandCompactOptions userArgs
  go defaultState (Array.toUnfoldable expanded)
  where
  go :: ParseState -> List String -> Effect ParseState
  go state Nil = case state.srcFile of
    Nothing -> do
      error "Error: no source file specified"
      error ""
      error "Usage: clodpurs [options] <file>"
      exit' 1
    Just _ -> pure state
  go state (Cons arg rest) = case arg of
    "--lex" -> go (state { stage = Lex }) rest
    "--parse" -> go (state { stage = Parse }) rest
    "--validate" -> go (state { stage = Validate }) rest
    "--tacky" -> go (state { stage = Tacky }) rest
    "--codegen" -> go (state { stage = Codegen }) rest
    "-S" -> go (state { stage = Assembly }) rest
    "-s" -> go (state { stage = Assembly }) rest
    "-c" -> go (state { stage = Obj }) rest
    "-d" -> go (state { debug = true }) rest
    "-o" -> go (state { optimize = true }) rest
    "--optimize" -> go (state { optimize = true }) rest
    "--fold-constants" -> go (state { foldConstants = true }) rest
    "--eliminate-dead-stores" -> go (state { eliminateDeadStores = true }) rest
    "--propagate-copies" -> go (state { propagateCopies = true }) rest
    "--eliminate-unreachable-code" -> go (state { eliminateUnreachableCode = true }) rest
    "-t" -> case rest of
      Cons val rest' -> case val of
        "linux" -> go (state { target = Linux }) rest'
        "osx" -> go (state { target = OS_X }) rest'
        _ -> do
          error ("Error: invalid target: " <> val <> " (expected 'linux' or 'osx')")
          exit' 1
      Nil -> do
        error "Error: -t requires a value"
        exit' 1
    "--target" -> case rest of
      Cons val rest' -> case val of
        "linux" -> go (state { target = Linux }) rest'
        "osx" -> go (state { target = OS_X }) rest'
        _ -> do
          error ("Error: invalid target: " <> val <> " (expected 'linux' or 'osx')")
          exit' 1
      Nil -> do
        error "Error: --target requires a value"
        exit' 1
    "-l" -> case rest of
      Cons lib rest' -> go (state { libs = state.libs <> [lib] }) rest'
      Nil -> do
        error "Error: -l requires a library name"
        exit' 1
    _ ->
      if String.take 1 arg == "-" then do
        error ("Error: unknown option: " <> arg)
        exit' 1
      else
        go (state { srcFile = Just arg }) rest

-- | Expand compact option forms like -lm into -l m
expandCompactOptions :: Array String -> Array String
expandCompactOptions = Array.concatMap \arg ->
  if String.take 2 arg == "-l" && String.length arg > 2 then
    ["-l", String.drop 2 arg]
  else
    [arg]

main :: Effect Unit
main = do
  args <- argv
  state <- parseArgs args
  case state.srcFile of
    Nothing -> do
      error "Error: no source file specified"
      exit' 1
    Just src -> do
      fileExists <- exists src
      when (not fileExists) do
        error ("Error: file not found: " <> src)
        exit' 1
      let optimizations = setOptions
            state.optimize
            state.foldConstants
            state.eliminateDeadStores
            state.propagateCopies
            state.eliminateUnreachableCode
      driver state.target state.debug state.libs state.stage optimizations src
