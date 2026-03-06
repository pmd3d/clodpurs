module Emit where

import Prelude

import Assembly (AsmBinaryOperator(..), AsmCondCode(..), AsmInstruction(..), AsmOperand(..), AsmProgram(..), AsmReg(..), AsmTopLevel(..), AsmType(..), AsmUnaryOperator(..))
import AssemblySymbols (AsmSymbolTableMap)
import AssemblySymbols as AssemblySymbols
import CompilerError (CompilerError(..))
import Data.BigInt as BigInt
import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as CU
import Data.Tuple (Tuple(..))
import Initializers (StaticInit(..))
import Initializers as Initializers
import ResultUtil (resultTraverse)
import Settings (Target(..))
import StringUtil as StringUtil

foreign import doubleToInt64BitsImpl :: Number -> String

suffix :: AsmType -> Either CompilerError String
suffix = case _ of
  Byte -> Right "b"
  Longword -> Right "l"
  Quadword -> Right "q"
  AsmDouble -> Right "sd"
  ByteArray _ ->
    Left (InternalError "found instruction w/ non-scalar operand type")

alignDirective :: Target -> String
alignDirective = case _ of
  OS_X -> ".balign"
  Linux -> ".align"

showLabel :: Target -> String -> String
showLabel platform name = case platform of
  OS_X -> "_" <> name
  Linux -> name

showLocalLabel :: Target -> String -> String
showLocalLabel platform label = case platform of
  OS_X -> "L" <> label
  Linux -> ".L" <> label

showFunName :: Target -> AsmSymbolTableMap -> String -> Either CompilerError String
showFunName platform asmSymbols f = case platform of
  OS_X -> Right ("_" <> f)
  Linux -> do
    defined <- AssemblySymbols.isDefined f asmSymbols
    Right (if defined then f else f <> "@PLT")

showLongReg :: AsmReg -> Either CompilerError String
showLongReg = case _ of
  AX -> Right "%eax"
  BX -> Right "%ebx"
  CX -> Right "%ecx"
  DX -> Right "%edx"
  DI -> Right "%edi"
  SI -> Right "%esi"
  R8 -> Right "%r8d"
  R9 -> Right "%r9d"
  R10 -> Right "%r10d"
  R11 -> Right "%r11d"
  R12 -> Right "%r12d"
  R13 -> Right "%r13d"
  R14 -> Right "%r14d"
  R15 -> Right "%r15d"
  SP -> Left (InternalError "no 32-bit RSP")
  BP -> Left (InternalError "no 32-bit RBP")
  _ -> Left (InternalError "can't store longword type in XMM register")

showQuadwordReg :: AsmReg -> Either CompilerError String
showQuadwordReg = case _ of
  AX -> Right "%rax"
  BX -> Right "%rbx"
  CX -> Right "%rcx"
  DX -> Right "%rdx"
  DI -> Right "%rdi"
  SI -> Right "%rsi"
  R8 -> Right "%r8"
  R9 -> Right "%r9"
  R10 -> Right "%r10"
  R11 -> Right "%r11"
  R12 -> Right "%r12"
  R13 -> Right "%r13"
  R14 -> Right "%r14"
  R15 -> Right "%r15"
  SP -> Right "%rsp"
  BP -> Right "%rbp"
  _ -> Left (InternalError "can't store quadword type in XMM register")

showDoubleReg :: AsmReg -> Either CompilerError String
showDoubleReg = case _ of
  XMM0 -> Right "%xmm0"
  XMM1 -> Right "%xmm1"
  XMM2 -> Right "%xmm2"
  XMM3 -> Right "%xmm3"
  XMM4 -> Right "%xmm4"
  XMM5 -> Right "%xmm5"
  XMM6 -> Right "%xmm6"
  XMM7 -> Right "%xmm7"
  XMM8 -> Right "%xmm8"
  XMM9 -> Right "%xmm9"
  XMM10 -> Right "%xmm10"
  XMM11 -> Right "%xmm11"
  XMM12 -> Right "%xmm12"
  XMM13 -> Right "%xmm13"
  XMM14 -> Right "%xmm14"
  XMM15 -> Right "%xmm15"
  _ -> Left (InternalError "can't store double type in general-purpose register")

showByteReg :: AsmReg -> Either CompilerError String
showByteReg = case _ of
  AX -> Right "%al"
  BX -> Right "%bl"
  CX -> Right "%cl"
  DX -> Right "%dl"
  DI -> Right "%dil"
  SI -> Right "%sil"
  R8 -> Right "%r8b"
  R9 -> Right "%r9b"
  R10 -> Right "%r10b"
  R11 -> Right "%r11b"
  R12 -> Right "%r12b"
  R13 -> Right "%r13b"
  R14 -> Right "%r14b"
  R15 -> Right "%r15b"
  SP -> Left (InternalError "no one-byte RSP")
  BP -> Left (InternalError "no one-byte RBP")
  _ -> Left (InternalError "can't store byte type in XMM register")

showOperand :: Target -> AsmSymbolTableMap -> AsmType -> AsmOperand -> Either CompilerError String
showOperand platform asmSymbols t = case _ of
  Reg r ->
    case t of
      Byte -> showByteReg r
      Longword -> showLongReg r
      Quadword -> showQuadwordReg r
      AsmDouble -> showDoubleReg r
      ByteArray _ -> Left (InternalError "can't store non-scalar operand in register")
  Imm i -> Right ("$" <> show i)
  Memory r 0 -> do
    rStr <- showQuadwordReg r
    Right ("(" <> rStr <> ")")
  Memory r i -> do
    rStr <- showQuadwordReg r
    Right (show i <> "(" <> rStr <> ")")
  Data name offset -> do
    isConst <- AssemblySymbols.isConstant name asmSymbols
    let lbl = if isConst then showLocalLabel platform name
              else showLabel platform name
    if offset == 0 then Right (lbl <> "(%rip)")
    else Right (lbl <> "+" <> show offset <> "(%rip)")
  Indexed { baseReg: b, index: idx, scale } -> do
    bStr <- showQuadwordReg b
    indexStr <- showQuadwordReg idx
    Right ("(" <> bStr <> ", " <> indexStr <> ", " <> show scale <> ")")
  -- printing out pseudoregisters is only for debugging
  Pseudo name -> Right ("%" <> name)
  PseudoMem name offset -> Right (show offset <> "(%" <> name <> ")")

showByteOperand :: Target -> AsmSymbolTableMap -> AsmOperand -> Either CompilerError String
showByteOperand platform asmSymbols = case _ of
  Reg r -> showByteReg r
  other -> showOperand platform asmSymbols Longword other

showUnaryInstruction :: AsmUnaryOperator -> String
showUnaryInstruction = case _ of
  Neg -> "neg"
  Not -> "not"
  Shr -> "shr"

showBinaryInstruction :: AsmBinaryOperator -> Either CompilerError String
showBinaryInstruction = case _ of
  Add -> Right "add"
  Sub -> Right "sub"
  Mult -> Right "imul"
  DivDouble -> Right "div"
  And -> Right "and"
  Or -> Right "or"
  Shl -> Right "shl"
  ShrBinop -> Right "shr"
  Xor -> Left (InternalError "should handle xor as special case")

showCondCode :: AsmCondCode -> String
showCondCode = case _ of
  E -> "e"
  NE -> "ne"
  G -> "g"
  GE -> "ge"
  L -> "l"
  LE -> "le"
  A -> "a"
  AE -> "ae"
  B -> "b"
  BE -> "be"

emitInstruction :: Target -> AsmSymbolTableMap -> AsmInstruction -> Either CompilerError String
emitInstruction platform asmSymbols = case _ of
  Mov t src dst -> do
    sfx <- suffix t
    srcStr <- showOperand platform asmSymbols t src
    dstStr <- showOperand platform asmSymbols t dst
    Right ("\tmov" <> sfx <> " " <> srcStr <> ", " <> dstStr <> "\n")
  Unary operator t dst -> do
    sfx <- suffix t
    dstStr <- showOperand platform asmSymbols t dst
    Right ("\t" <> showUnaryInstruction operator <> sfx <> " " <> dstStr <> "\n")
  Binary { op: Xor, t: AsmDouble, src, dst } -> do
    srcStr <- showOperand platform asmSymbols AsmDouble src
    dstStr <- showOperand platform asmSymbols AsmDouble dst
    Right ("\txorpd " <> srcStr <> ", " <> dstStr <> "\n")
  Binary { op: Mult, t: AsmDouble, src, dst } -> do
    srcStr <- showOperand platform asmSymbols AsmDouble src
    dstStr <- showOperand platform asmSymbols AsmDouble dst
    Right ("\tmulsd " <> srcStr <> ", " <> dstStr <> "\n")
  Binary { op, t, src, dst } -> do
    opStr <- showBinaryInstruction op
    sfx <- suffix t
    srcStr <- showOperand platform asmSymbols t src
    dstStr <- showOperand platform asmSymbols t dst
    Right ("\t" <> opStr <> sfx <> " " <> srcStr <> ", " <> dstStr <> "\n")
  Cmp AsmDouble src dst -> do
    srcStr <- showOperand platform asmSymbols AsmDouble src
    dstStr <- showOperand platform asmSymbols AsmDouble dst
    Right ("\tcomisd " <> srcStr <> ", " <> dstStr <> "\n")
  Cmp t src dst -> do
    sfx <- suffix t
    srcStr <- showOperand platform asmSymbols t src
    dstStr <- showOperand platform asmSymbols t dst
    Right ("\tcmp" <> sfx <> " " <> srcStr <> ", " <> dstStr <> "\n")
  Idiv t operand -> do
    sfx <- suffix t
    opStr <- showOperand platform asmSymbols t operand
    Right ("\tidiv" <> sfx <> " " <> opStr <> "\n")
  Div t operand -> do
    sfx <- suffix t
    opStr <- showOperand platform asmSymbols t operand
    Right ("\tdiv" <> sfx <> " " <> opStr <> "\n")
  Lea src dst -> do
    srcStr <- showOperand platform asmSymbols Quadword src
    dstStr <- showOperand platform asmSymbols Quadword dst
    Right ("\tleaq " <> srcStr <> ", " <> dstStr <> "\n")
  Cdq Longword -> Right "\tcdq\n"
  Cdq Quadword -> Right "\tcqo\n"
  Jmp lbl -> Right ("\tjmp " <> showLocalLabel platform lbl <> "\n")
  JmpCC code lbl ->
    Right ("\tj" <> showCondCode code <> " " <> showLocalLabel platform lbl <> "\n")
  SetCC code operand -> do
    opStr <- showByteOperand platform asmSymbols operand
    Right ("\tset" <> showCondCode code <> " " <> opStr <> "\n")
  Label lbl -> Right (showLocalLabel platform lbl <> ":\n")
  Push op -> do
    opStr <- showOperand platform asmSymbols Quadword op
    Right ("\tpushq " <> opStr <> "\n")
  Pop r -> do
    rStr <- showQuadwordReg r
    Right ("\tpopq " <> rStr <> "\n")
  Call f -> do
    fName <- showFunName platform asmSymbols f
    Right ("\tcall " <> fName <> "\n")
  Movsx { src_type, dst_type, src, dst } -> do
    srcSfx <- suffix src_type
    dstSfx <- suffix dst_type
    srcStr <- showOperand platform asmSymbols src_type src
    dstStr <- showOperand platform asmSymbols dst_type dst
    Right ("\tmovs" <> srcSfx <> dstSfx <> " " <> srcStr <> ", " <> dstStr <> "\n")
  MovZeroExtend { src_type, dst_type, src, dst } -> do
    srcSfx <- suffix src_type
    dstSfx <- suffix dst_type
    srcStr <- showOperand platform asmSymbols src_type src
    dstStr <- showOperand platform asmSymbols dst_type dst
    Right ("\tmovz" <> srcSfx <> dstSfx <> " " <> srcStr <> ", " <> dstStr <> "\n")
  Cvtsi2sd t src dst -> do
    sfx <- suffix t
    srcStr <- showOperand platform asmSymbols t src
    dstStr <- showOperand platform asmSymbols AsmDouble dst
    Right ("\tcvtsi2sd" <> sfx <> " " <> srcStr <> ", " <> dstStr <> "\n")
  Cvttsd2si t src dst -> do
    sfx <- suffix t
    srcStr <- showOperand platform asmSymbols AsmDouble src
    dstStr <- showOperand platform asmSymbols t dst
    Right ("\tcvttsd2si" <> sfx <> " " <> srcStr <> ", " <> dstStr <> "\n")
  Ret -> Right "\n\tmovq %rbp, %rsp\n\tpopq %rbp\n\tret\n"
  Cdq _ -> Left (InternalError "can't apply cdq to a byte or non-integer type")

emitGlobalDirective :: Boolean -> String -> String
emitGlobalDirective isGlobal label =
  if isGlobal then "\t.globl " <> label <> "\n"
  else ""

foldlChars :: (String -> Char -> String) -> String -> String -> String
foldlChars f init str =
  let go acc idx
        | idx >= CU.length str = acc
        | otherwise = case CU.charAt idx str of
            Nothing -> acc
            Just c -> go (f acc c) (idx + 1)
  in go init 0

toOctal :: Int -> String
toOctal n
  | n < 8 = show n
  | otherwise = toOctal (n / 8) <> show (n `mod` 8)

padOctal :: String -> String
padOctal s
  | CU.length s >= 3 = s
  | CU.length s == 2 = "0" <> s
  | CU.length s == 1 = "00" <> s
  | otherwise = "000"

emitInit :: Target -> StaticInit -> String
emitInit platform = case _ of
  IntInit i -> "\t.long " <> show i <> "\n"
  LongInit l -> "\t.quad " <> BigInt.toString l <> "\n"
  UIntInit u -> "\t.long " <> BigInt.toString u <> "\n"
  ULongInit l -> "\t.quad " <> BigInt.toString l <> "\n"
  CharInit c -> "\t.byte " <> show c <> "\n"
  UCharInit uc -> "\t.byte " <> show uc <> "\n"
  DoubleInit d -> "\t.quad " <> doubleToInt64BitsImpl d <> "\n"
  ZeroInit byte_count -> "\t.zero " <> show byte_count <> "\n"
  StringInit s true -> "\t.asciz \"" <> escapeString s <> "\"\n"
  StringInit s false -> "\t.ascii \"" <> escapeString s <> "\"\n"
  PointerInit lbl -> "\t.quad " <> showLocalLabel platform lbl <> "\n"

escapeString :: String -> String
escapeString s = foldlChars (\acc c -> acc <> escapeChar c) "" s
  where
  escapeChar c =
    if StringUtil.isAlnum c then CU.singleton c
    else
      let code = toCharCode c
      in "\\" <> padOctal (toOctal code)

emitConstant :: Target -> String -> Int -> StaticInit -> Either CompilerError String
emitConstant platform name alignment init = do
  sectionName <- case platform of
    Linux -> Right ".section .rodata"
    OS_X -> case init of
      StringInit _ _ -> Right ".cstring"
      _ ->
        if alignment == 8 then Right ".literal8"
        else if alignment == 16 then Right ".literal16"
        else Left (InternalError "found constant with bad alignment")
  let header = "\n\t" <> sectionName <> "\n\t" <> alignDirective platform <> " " <> show alignment
        <> "\n  " <> showLocalLabel platform name <> ":\n"
      body = emitInit platform init
      extra = if sectionName == ".literal16" then emitInit platform (LongInit (BigInt.fromInt 0)) else ""
  Right (header <> body <> extra)

emitTl :: Target -> AsmSymbolTableMap -> AsmTopLevel -> Either CompilerError String
emitTl platform asmSymbols = case _ of
  Function { name, isGlobal, instructions } -> do
    let label = showLabel platform name
        header = emitGlobalDirective isGlobal label
          <> "\n\t.text\n" <> label <> ":\n\tpushq %rbp\n\tmovq %rsp, %rbp\n"
    instrStrs <- resultTraverse (emitInstruction platform asmSymbols) instructions
    Right (header <> foldList instrStrs)
  StaticVariable { name, isGlobal, init, alignment }
    | allZero init -> do
    let label = showLabel platform name
        header = emitGlobalDirective isGlobal label
          <> "\n\t.bss\n\t" <> alignDirective platform <> " " <> show alignment <> "\n" <> label <> ":\n"
        body = foldList (map (emitInit platform) init)
    Right (header <> body)
  StaticVariable { name, isGlobal, init, alignment } -> do
    let label = showLabel platform name
        header = emitGlobalDirective isGlobal label
          <> "\n\t.data\n\t" <> alignDirective platform <> " " <> show alignment <> "\n" <> label <> ":\n"
        body = foldList (map (emitInit platform) init)
    Right (header <> body)
  StaticConstant { name, alignment, init } ->
    emitConstant platform name alignment init

allZero :: List StaticInit -> Boolean
allZero Nil = true
allZero (Cons x rest) = Initializers.isZero x && allZero rest

foldList :: List String -> String
foldList Nil = ""
foldList (Cons x rest) = x <> foldList rest

emitStackNote :: Target -> String
emitStackNote = case _ of
  OS_X -> ""
  Linux -> "\t.section .note.GNU-stack,\"\",@progbits\n"

emitToString :: Target -> AsmSymbolTableMap -> AsmProgram -> Either CompilerError String
emitToString platform asmSymbols (Program tls) = do
  tlStrs <- resultTraverse (emitTl platform asmSymbols) tls
  Right (foldList tlStrs <> emitStackNote platform)
