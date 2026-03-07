module InstructionFixup where

import Prelude

import Assembly (AsmBinaryOperator(..), AsmInstruction(..), AsmOperand(..), AsmProgram(..), AsmReg(..), AsmTopLevel(..), AsmType(..))
import AssemblySymbols (AsmSymbolTableMap)
import AssemblySymbols as AssemblySymbols
import CompilerError (CompilerError)
import Data.BigInt as BigInt
import Data.BigInt (BigInt)
import Data.Either (Either(..))
import Data.List (List(..), (:), concatMap, reverse)
import Data.List as List
import Data.Maybe (fromMaybe)
import RegSet as RegSet
import ResultUtil (resultTraverse)
import Rounding as Rounding

int32_max :: BigInt
int32_max = BigInt.fromInt 2147483647

int32_min :: BigInt
int32_min = BigInt.fromInt (-2147483648)

isLarge :: BigInt -> Boolean
isLarge imm = imm > int32_max || imm < int32_min

uint32_max :: BigInt
uint32_max = fromMaybe (BigInt.fromInt 0) (BigInt.fromString "4294967295")

isLargerThanUint :: BigInt -> Boolean
isLargerThanUint imm =
  -- use unsigned upper-bound for positives, signed 32-bit lower bound for negatives
  imm > uint32_max || imm < int32_min

isLargerThanByte :: BigInt -> Boolean
isLargerThanByte imm = imm >= BigInt.fromInt 256 || imm < BigInt.fromInt (-128)

isConstant :: AsmOperand -> Boolean
isConstant = case _ of
  Imm _ -> true
  _ -> false

isMemory :: AsmOperand -> Boolean
isMemory = case _ of
  Memory _ _ -> true
  Data _ _ -> true
  Indexed _ -> true
  _ -> false

isXmm :: AsmReg -> Boolean
isXmm = case _ of
  XMM0 -> true
  XMM1 -> true
  XMM2 -> true
  XMM3 -> true
  XMM4 -> true
  XMM5 -> true
  XMM6 -> true
  XMM7 -> true
  XMM8 -> true
  XMM9 -> true
  XMM10 -> true
  XMM11 -> true
  XMM12 -> true
  XMM13 -> true
  XMM14 -> true
  XMM15 -> true
  _ -> false

fixupInstruction :: List AsmReg -> AsmInstruction -> List AsmInstruction
fixupInstruction callee_saved_regs = case _ of
  -- Mov can't move a value from one memory address to another
  Mov t src dst | isMemory src && isMemory dst ->
    let scratch = if t == AsmDouble then Reg XMM14 else Reg R10
    in Mov t src scratch : Mov t scratch dst : Nil
  -- Mov can't move a large constant to a memory address
  Mov Quadword (Imm i) dst | isLarge i && isMemory dst ->
    Mov Quadword (Imm i) (Reg R10) : Mov Quadword (Reg R10) dst : Nil
  -- Moving a quadword-size constant with a longword operand size produces assembler warning
  Mov Longword (Imm i) dst | isLargerThanUint i ->
    -- reduce modulo 2^32 by zeroing out upper 32 bits
    let reduced = BigInt.and i uint32_max
    in Mov Longword (Imm reduced) dst : Nil
  -- Moving a longword-size constant with a byte operand size produces assembler warning
  Mov Byte (Imm i) dst | isLargerThanByte i ->
    let masked = BigInt.and i (BigInt.fromInt 255)
        reduced = if masked >= BigInt.fromInt 128 then masked - BigInt.fromInt 256 else masked
    in Mov Byte (Imm reduced) dst : Nil
  -- Movsx can't handle immediate source or memory dst
  Movsx { src_type, dst_type, src: src@(Imm _), dst } | isMemory dst ->
    Mov src_type src (Reg R10)
      : Movsx { src_type, dst_type, src: Reg R10, dst: Reg R11 }
      : Mov dst_type (Reg R11) dst
      : Nil
  Movsx { src_type, dst_type, src: src@(Imm _), dst } ->
    Mov src_type src (Reg R10)
      : Movsx { src_type, dst_type, src: Reg R10, dst }
      : Nil
  Movsx { src_type, dst_type, src, dst } | isMemory dst ->
    Movsx { src_type, dst_type, src, dst: Reg R11 }
      : Mov dst_type (Reg R11) dst
      : Nil
  -- MovZeroExtend src can't be an immediate (Byte case)
  MovZeroExtend { src_type: Byte, src: Imm i, dst_type, dst } ->
    if isMemory dst then
      Mov Byte (Imm i) (Reg R10)
        : MovZeroExtend { src_type: Byte, src: Reg R10, dst_type, dst: Reg R11 }
        : Mov dst_type (Reg R11) dst
        : Nil
    else
      Mov Byte (Imm i) (Reg R10)
        : MovZeroExtend { src_type: Byte, src: Reg R10, dst_type, dst }
        : Nil
  -- MovZeroExtend destination must be a register (Byte case)
  MovZeroExtend { src_type: Byte, dst_type, src, dst } | isMemory dst ->
    MovZeroExtend { src_type: Byte, dst_type, src, dst: Reg R11 }
      : Mov dst_type (Reg R11) dst
      : Nil
  -- to zero-extend longword to quadword, first copy into register, then move to destination
  MovZeroExtend { src_type: Longword, dst_type, src, dst } | isMemory dst ->
    Mov Longword src (Reg R11) : Mov dst_type (Reg R11) dst : Nil
  -- if destination is already a register, zero-extend w/ a single mov instruction
  MovZeroExtend { src_type: Longword, src, dst } ->
    Mov Longword src dst : Nil
  -- Idiv can't operate on constants
  Idiv t (Imm i) -> Mov t (Imm i) (Reg R10) : Idiv t (Reg R10) : Nil
  Div t (Imm i) -> Mov t (Imm i) (Reg R10) : Div t (Reg R10) : Nil
  -- dst of lea must be a register
  Lea src dst | isMemory dst ->
    Lea src (Reg R11) : Mov Quadword (Reg R11) dst : Nil
  -- Binary operations on double require register as destination
  i@(Binary { t: AsmDouble, dst: Reg _ }) -> i : Nil
  Binary { op, t: AsmDouble, src, dst } ->
    Mov AsmDouble dst (Reg XMM15)
      : Binary { op, t: AsmDouble, src, dst: Reg XMM15 }
      : Mov AsmDouble (Reg XMM15) dst
      : Nil
  -- Add/Sub/And/Or can't take large immediates as source operands
  Binary { op, t: Quadword, src: src@(Imm i), dst }
    | (op == Add || op == Sub || op == And || op == Or) && isLarge i ->
    Mov Quadword src (Reg R10)
      : Binary { op, t: Quadword, src: Reg R10, dst }
      : Nil
  -- Add/Sub can't use memory addresses for both operands
  Binary { op, t, src, dst }
    | (op == Add || op == Sub || op == And || op == Or) && isMemory src && isMemory dst ->
    Mov t src (Reg R10)
      : Binary { op, t, src: Reg R10, dst }
      : Nil
  -- Destination of Mult can't be in memory; src can't be a big operand
  Binary { op: Mult, t: Quadword, src: src@(Imm i), dst }
    | isLarge i && isMemory dst ->
    Mov Quadword src (Reg R10)
      : Mov Quadword dst (Reg R11)
      : Binary { op: Mult, t: Quadword, src: Reg R10, dst: Reg R11 }
      : Mov Quadword (Reg R11) dst
      : Nil
  Binary { op: Mult, t: Quadword, src: src@(Imm i), dst } | isLarge i ->
    Mov Quadword src (Reg R10)
      : Binary { op: Mult, t: Quadword, src: Reg R10, dst }
      : Nil
  Binary { op: Mult, t, src, dst } | isMemory dst ->
    Mov t dst (Reg R11)
      : Binary { op: Mult, t, src, dst: Reg R11 }
      : Mov t (Reg R11) dst
      : Nil
  -- destination of comisd must be a register
  i@(Cmp AsmDouble _ (Reg _)) -> i : Nil
  Cmp AsmDouble src dst ->
    Mov AsmDouble dst (Reg XMM15) : Cmp AsmDouble src (Reg XMM15) : Nil
  -- Both operands of cmp can't be in memory
  Cmp t src dst | isMemory src && isMemory dst ->
    Mov t src (Reg R10) : Cmp t (Reg R10) dst : Nil
  -- first operand of Cmp can't be a large constant, second can't be a constant at all
  Cmp Quadword src@(Imm i) dst@(Imm _) | isLarge i ->
    Mov Quadword src (Reg R10)
      : Mov Quadword dst (Reg R11)
      : Cmp Quadword (Reg R10) (Reg R11)
      : Nil
  Cmp Quadword src@(Imm i) dst | isLarge i ->
    Mov Quadword src (Reg R10) : Cmp Quadword (Reg R10) dst : Nil
  Cmp t src (Imm i) ->
    Mov t (Imm i) (Reg R11) : Cmp t src (Reg R11) : Nil
  Push (Reg r) | isXmm r ->
    Binary { op: Sub, t: Quadword, src: Imm (BigInt.fromInt 8), dst: Reg SP }
      : Mov AsmDouble (Reg r) (Memory SP 0)
      : Nil
  Push src@(Imm i) | isLarge i ->
    Mov Quadword src (Reg R10) : Push (Reg R10) : Nil
  -- destination of cvttsd2si must be a register
  Cvttsd2si t src dst | isMemory dst ->
    Cvttsd2si t src (Reg R11) : Mov t (Reg R11) dst : Nil
  Cvtsi2sd t src dst
    | isConstant src && isMemory dst ->
    Mov t src (Reg R10)
      : Cvtsi2sd t (Reg R10) (Reg XMM15)
      : Mov AsmDouble (Reg XMM15) dst
      : Nil
  Cvtsi2sd t src dst | isConstant src ->
    Mov t src (Reg R10) : Cvtsi2sd t (Reg R10) dst : Nil
  Cvtsi2sd t src dst | isMemory dst ->
    Cvtsi2sd t src (Reg XMM15) : Mov AsmDouble (Reg XMM15) dst : Nil
  Ret ->
    let restore_regs = map Pop (reverse callee_saved_regs)
    in restore_regs <> (Ret : Nil)
  other -> other : Nil

emitStackAdjustment :: Int -> Int -> AsmInstruction
emitStackAdjustment bytes_for_locals callee_saved_count =
  let callee_saved_bytes = 8 * callee_saved_count
      total_stack_bytes = callee_saved_bytes + bytes_for_locals
      adjusted_stack_bytes = Rounding.roundAwayFromZero 16 total_stack_bytes
      stack_adjustment = adjusted_stack_bytes - callee_saved_bytes
  in Binary { op: Sub, t: Quadword, src: Imm (BigInt.fromInt stack_adjustment), dst: Reg SP }

fixupTl :: AsmSymbolTableMap -> AsmTopLevel -> Either CompilerError AsmTopLevel
fixupTl asmSymbols = case _ of
  Function { name, isGlobal, instructions } -> do
    bytes_req <- AssemblySymbols.getBytesRequired name asmSymbols
    let stack_bytes = -bytes_req
    callee_saved_regs <- map RegSet.elements (AssemblySymbols.getCalleeSavedRegsUsed name asmSymbols)
    let save_reg r = Push (Reg r)
        adjust_rsp = emitStackAdjustment stack_bytes (List.length callee_saved_regs)
        setup_instructions = adjust_rsp : map save_reg callee_saved_regs
    Right (Function
      { name
      , isGlobal
      , instructions:
          setup_instructions
          <> concatMap (fixupInstruction callee_saved_regs) instructions
      })
  static_var -> Right static_var

fixupProgram :: AsmSymbolTableMap -> AsmProgram -> Either CompilerError AsmProgram
fixupProgram asmSymbols (Program tls) = do
  fixed_functions <- resultTraverse (fixupTl asmSymbols) tls
  Right (Program fixed_functions)
