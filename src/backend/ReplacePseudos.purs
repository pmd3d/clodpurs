module ReplacePseudos where

import Prelude

import Assembly (AsmInstruction(..), AsmOperand(..), AsmProgram(..), AsmReg(..), AsmTopLevel(..), AsmType(..))
import AssemblySymbols (AsmSymbolTableMap)
import AssemblySymbols as AssemblySymbols
import CompilerError (CompilerError(..))
import Data.Either (Either(..))
import Data.List (List(..), (:), reverse)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import ResultUtil (resultFold)
import Rounding as Rounding

-- Structure to keep track of what stack slots we've assigned so far
type ReplacementState =
  { current_offset :: Int -- last used stack slot
  , offset_map :: Map.Map String Int -- map from pseudoregister to stack slots
  }

calculateOffset :: AsmSymbolTableMap -> ReplacementState -> String -> Either CompilerError (Tuple ReplacementState Int)
calculateOffset asmSymbols state name = do
  size <- AssemblySymbols.getSize name asmSymbols
  alignment <- AssemblySymbols.getAlignment name asmSymbols
  let new_offset = Rounding.roundAwayFromZero alignment (state.current_offset - size)
  let new_state =
        { current_offset: new_offset
        , offset_map: Map.insert name new_offset state.offset_map
        }
  Right (Tuple new_state new_offset)

replaceOperand :: AsmSymbolTableMap -> ReplacementState -> AsmOperand -> Either CompilerError (Tuple ReplacementState AsmOperand)
replaceOperand asmSymbols state = case _ of
  -- if it's a pseudoregister, replace it with a stack slot
  Pseudo s -> do
    isStatic <- AssemblySymbols.isStatic s asmSymbols
    if isStatic then Right (Tuple state (Data s 0))
    else
      case Map.lookup s state.offset_map of
        -- We've already assigned this operand a stack slot
        Just offset -> Right (Tuple state (Memory BP offset))
        -- We haven't already assigned it a stack slot;
        -- assign it and update state
        Nothing -> do
          Tuple new_state new_offset <- calculateOffset asmSymbols state s
          Right (Tuple new_state (Memory BP new_offset))
  PseudoMem s offset -> do
    isStatic <- AssemblySymbols.isStatic s asmSymbols
    if isStatic then
      Right (Tuple state (Data s offset))
    else
      case Map.lookup s state.offset_map of
        -- We've already assigned this operand a stack slot
        Just var_offset -> Right (Tuple state (Memory BP (offset + var_offset)))
        Nothing -> do
          -- assign s a stack slot, and add its offset to the offset w/in s to
          -- get new operand
          Tuple new_state new_var_offset <- calculateOffset asmSymbols state s
          Right (Tuple new_state (Memory BP (offset + new_var_offset)))
  -- not a pseudo, so nothing to do
  other -> Right (Tuple state other)

replacePseudosInInstruction :: AsmSymbolTableMap -> ReplacementState -> AsmInstruction -> Either CompilerError (Tuple ReplacementState AsmInstruction)
replacePseudosInInstruction asmSymbols state = case _ of
  -- Replace src and dst of mov instruction
  Mov t src dst -> do
    Tuple state1 new_src <- replaceOperand asmSymbols state src
    Tuple state2 new_dst <- replaceOperand asmSymbols state1 dst
    Right (Tuple state2 (Mov t new_src new_dst))
  Movsx fields -> do
    Tuple state1 new_src <- replaceOperand asmSymbols state fields.src
    Tuple state2 new_dst <- replaceOperand asmSymbols state1 fields.dst
    Right (Tuple state2 (Movsx (fields { src = new_src, dst = new_dst })))
  MovZeroExtend fields -> do
    Tuple state1 new_src <- replaceOperand asmSymbols state fields.src
    Tuple state2 new_dst <- replaceOperand asmSymbols state1 fields.dst
    Right (Tuple state2 (MovZeroExtend (fields { src = new_src, dst = new_dst })))
  Lea src dst -> do
    Tuple state1 new_src <- replaceOperand asmSymbols state src
    Tuple state2 new_dst <- replaceOperand asmSymbols state1 dst
    Right (Tuple state2 (Lea new_src new_dst))
  -- Replace dst of unary instruction
  -- Note: F# has Unary (t, op, dst) but PureScript Assembly has Unary op t dst
  Unary op t dst -> do
    Tuple state1 new_dst <- replaceOperand asmSymbols state dst
    Right (Tuple state1 (Unary op t new_dst))
  Binary { op, t, src, dst } -> do
    Tuple state1 new_src <- replaceOperand asmSymbols state src
    Tuple state2 new_dst <- replaceOperand asmSymbols state1 dst
    Right (Tuple state2 (Binary { op, t, src: new_src, dst: new_dst }))
  Cmp t op1 op2 -> do
    Tuple state1 new_op1 <- replaceOperand asmSymbols state op1
    Tuple state2 new_op2 <- replaceOperand asmSymbols state1 op2
    Right (Tuple state2 (Cmp t new_op1 new_op2))
  Idiv t op -> do
    Tuple state1 new_op <- replaceOperand asmSymbols state op
    Right (Tuple state1 (Idiv t new_op))
  Div t op -> do
    Tuple state1 new_op <- replaceOperand asmSymbols state op
    Right (Tuple state1 (Div t new_op))
  SetCC code op -> do
    Tuple state1 new_op <- replaceOperand asmSymbols state op
    Right (Tuple state1 (SetCC code new_op))
  Push op -> do
    Tuple state1 new_op <- replaceOperand asmSymbols state op
    Right (Tuple state1 (Push new_op))
  Cvttsd2si t src dst -> do
    Tuple state1 new_src <- replaceOperand asmSymbols state src
    Tuple state2 new_dst <- replaceOperand asmSymbols state1 dst
    Right (Tuple state2 (Cvttsd2si t new_src new_dst))
  Cvtsi2sd t src dst -> do
    Tuple state1 new_src <- replaceOperand asmSymbols state src
    Tuple state2 new_dst <- replaceOperand asmSymbols state1 dst
    Right (Tuple state2 (Cvtsi2sd t new_src new_dst))
  -- Ret, Cdq, Label, JmpCC, Jmp, Call have no operands to rewrite
  other@Ret -> Right (Tuple state other)
  other@(Cdq _) -> Right (Tuple state other)
  other@(Label _) -> Right (Tuple state other)
  other@(JmpCC _ _) -> Right (Tuple state other)
  other@(Jmp _) -> Right (Tuple state other)
  other@(Call _) -> Right (Tuple state other)
  Pop _ -> Left (InternalError "Internal error")

replacePseudosInTl :: AsmSymbolTableMap -> AsmTopLevel -> Either CompilerError (Tuple AsmSymbolTableMap AsmTopLevel)
replacePseudosInTl asmSymbols = case _ of
  Function { name, isGlobal, instructions } -> do
    returnsOnStack <- AssemblySymbols.returnsOnStack name asmSymbols
    let starting_offset = if returnsOnStack then -8 else 0
    let init_state = { current_offset: starting_offset, offset_map: Map.empty }
    -- rewrite each instruction, tracking current offset/stack slot map as we go
    Tuple final_state rev_instructions <-
      resultFold (\(Tuple st acc) i -> do
          Tuple st' i' <- replacePseudosInInstruction asmSymbols st i
          Right (Tuple st' (i' : acc))
        ) (Tuple init_state Nil) instructions
    let fixed_instructions = reverse rev_instructions
    asmSymbols' <- AssemblySymbols.setBytesRequired name final_state.current_offset asmSymbols
    Right (Tuple asmSymbols' (Function { name, isGlobal, instructions: fixed_instructions }))
  static_var -> Right (Tuple asmSymbols static_var)

replacePseudos :: AsmSymbolTableMap -> AsmProgram -> Either CompilerError (Tuple AsmSymbolTableMap AsmProgram)
replacePseudos asmSymbols (Program tls) =
  map (\(Tuple syms tls') -> Tuple syms (Program tls'))
    ( resultFold (\(Tuple syms acc) tl -> do
          Tuple syms' tl' <- replacePseudosInTl syms tl
          Right (Tuple syms' (acc <> Cons tl' Nil))
        ) (Tuple asmSymbols Nil) tls
    )
