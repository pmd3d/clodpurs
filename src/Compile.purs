module Compile where

import Prelude

import AddressTaken as AddressTaken
import AssemblySymbols as AssemblySymbols
import CompilerError (CompilerError(..))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as CU
import Data.Tuple (Tuple(..))
import Emit as Emit
import InstructionFixup as InstructionFixup
import LabelLoops as LabelLoops
import Lex as Lex
import Optimize as Optimize
import Parse as Parse
import ReplacePseudos as ReplacePseudos
import Resolve as Resolve
import Settings (CompilerConfig, Optimizations, Stage(..))
import Symbols as Symbols
import TackyPrint as TackyPrint
import Typecheck as Typecheck
import TypeTable as TypeTable
import UniqueIds as UniqueIds
import TackyGen as TackyGen
import Codegen as Codegen
import Regalloc as Regalloc

type CompileResult =
  { assemblyFile :: Maybe (Tuple String String) -- (path, content)
  , debugFiles :: List (Tuple String String) -- (path, content)
  , graphvizDots :: List (Tuple String String) -- (label, dotContent)
  }

emptyResult :: CompileResult
emptyResult = { assemblyFile: Nothing, debugFiles: Nil, graphvizDots: Nil }

compile :: CompilerConfig -> Stage -> Optimizations -> String -> String -> Either CompilerError CompileResult
compile config stage optimizations src_file source = do
  let counter = UniqueIds.initialCounter
  -- Lex it
  tokens <- lmap LexError (Lex.lex source)
  if stage == Lex then Right emptyResult
  else do
    ast <- lmap ParseError (Parse.parse tokens)
    if stage == Parse then Right emptyResult
    else do
      -- Semantic analysis:
      -- 1. resolve identifiers
      Tuple counter' resolved_ast <- lmap ResolveError (Resolve.resolve counter ast)
      -- 2. annotate loops and break/continue statements
      Tuple counter'' annotated_ast <- LabelLoops.labelLoops counter' resolved_ast
      -- 3. typecheck
      let initState = { counter: counter'', st: Symbols.empty, tt: TypeTable.empty }
      Tuple tchkState typed_ast <- Typecheck.typecheck initState annotated_ast
      if stage == Validate then Right emptyResult
      else do
        -- Convert the AST to TACKY
        { counter: counter''', st: st', program: tacky } <- TackyGen.gen tchkState.counter tchkState.st tchkState.tt typed_ast
        -- collect debug tacky output
        let Tuple counter4 tackyDebug = TackyPrint.debugPrintTacky config.debug counter''' src_file tacky
        let debugFiles = case tackyDebug of
              Just (Tuple path content) -> Cons (Tuple path content) Nil
              Nothing -> Nil
        -- optimize it!
        let optimized_tacky = Optimize.optimize optimizations src_file tacky
        if stage == Tacky then Right { assemblyFile: Nothing, debugFiles, graphvizDots: Nil }
        else do
          -- get all aliased vars for register allocation
          let aliased_vars = AddressTaken.analyzeProgram optimized_tacky
          -- Assembly generation:
          -- 1. convert TACKY to assembly
          Tuple _counter5 (Tuple asmSymbols asm_ast) <- Codegen.gen counter4 tchkState.tt st' AssemblySymbols.empty optimized_tacky
          -- collect pre-pseudoreg-allocation assembly if debug enabled
          debugFiles2 <-
            if config.debug then do
              let prealloc_filename = changeExtension ".prealloc.debug.s" src_file
              content <- Emit.emitToString config.platform asmSymbols asm_ast
              Right (debugFiles <> Cons (Tuple prealloc_filename content) Nil)
            else Right debugFiles
          -- 2. register allocation
          Tuple _counter6 (Tuple asmSymbols' (Tuple asm_ast1 graphvizDots)) <- Regalloc.allocateRegisters config.debug _counter5 asmSymbols aliased_vars asm_ast
          -- collect post-allocation assembly if debug enabled
          debugFiles3 <-
            if config.debug then do
              let postalloc_filename = changeExtension ".postalloc.debug.s" src_file
              content <- Emit.emitToString config.platform asmSymbols' asm_ast
              Right (debugFiles2 <> Cons (Tuple postalloc_filename content) Nil)
            else Right debugFiles2
          -- 3. replace pseudoregisters
          Tuple asmSymbols'' asm_ast2 <- ReplacePseudos.replacePseudos asmSymbols' asm_ast1
          -- 4. fix up instructions
          asm_ast3 <- InstructionFixup.fixupProgram asmSymbols'' asm_ast2
          if stage == Codegen then Right { assemblyFile: Nothing, debugFiles: debugFiles3, graphvizDots }
          else do
            let asm_filename = changeExtension ".s" src_file
            content <- Emit.emitToString config.platform asmSymbols'' asm_ast3
            Right { assemblyFile: Just (Tuple asm_filename content), debugFiles: debugFiles3, graphvizDots }

-- Path utilities
changeExtension :: String -> String -> String
changeExtension newExt path =
  case lastDotIndex path of
    Nothing -> path <> newExt
    Just idx -> CU.take idx path <> newExt

getFileNameWithoutExtension :: String -> String
getFileNameWithoutExtension path =
  let name = getFileName path
  in case lastDotIndex name of
    Nothing -> name
    Just idx -> CU.take idx name

getFileName :: String -> String
getFileName path =
  case lastSlashIndex path of
    Nothing -> path
    Just idx -> CU.drop (idx + 1) path

lastDotIndex :: String -> Maybe Int
lastDotIndex s = go (CU.length s - 1)
  where
  go i
    | i < 0 = Nothing
    | otherwise = case CU.charAt i s of
        Just '.' -> Just i
        _ -> go (i - 1)

lastSlashIndex :: String -> Maybe Int
lastSlashIndex s = go (CU.length s - 1)
  where
  go i
    | i < 0 = Nothing
    | otherwise = case CU.charAt i s of
        Just '/' -> Just i
        _ -> go (i - 1)
