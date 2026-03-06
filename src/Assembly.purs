module Assembly where

import Prelude

import Data.BigInt (BigInt)
import Data.List (List)
import Initializers (StaticInit)

data AsmReg
  = AX
  | BX
  | CX
  | DX
  | DI
  | SI
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  | SP
  | BP
  | XMM0
  | XMM1
  | XMM2
  | XMM3
  | XMM4
  | XMM5
  | XMM6
  | XMM7
  | XMM8
  | XMM9
  | XMM10
  | XMM11
  | XMM12
  | XMM13
  | XMM14
  | XMM15

derive instance eqAsmReg :: Eq AsmReg
derive instance ordAsmReg :: Ord AsmReg
instance showAsmReg :: Show AsmReg where
  show AX = "AX"
  show BX = "BX"
  show CX = "CX"
  show DX = "DX"
  show DI = "DI"
  show SI = "SI"
  show R8 = "R8"
  show R9 = "R9"
  show R10 = "R10"
  show R11 = "R11"
  show R12 = "R12"
  show R13 = "R13"
  show R14 = "R14"
  show R15 = "R15"
  show SP = "SP"
  show BP = "BP"
  show XMM0 = "XMM0"
  show XMM1 = "XMM1"
  show XMM2 = "XMM2"
  show XMM3 = "XMM3"
  show XMM4 = "XMM4"
  show XMM5 = "XMM5"
  show XMM6 = "XMM6"
  show XMM7 = "XMM7"
  show XMM8 = "XMM8"
  show XMM9 = "XMM9"
  show XMM10 = "XMM10"
  show XMM11 = "XMM11"
  show XMM12 = "XMM12"
  show XMM13 = "XMM13"
  show XMM14 = "XMM14"
  show XMM15 = "XMM15"

type AsmIndexedOperand = { baseReg :: AsmReg, index :: AsmReg, scale :: Int }

data AsmOperand
  = Imm BigInt
  | Reg AsmReg
  | Pseudo String
  | Memory AsmReg Int
  | Data String Int
  | PseudoMem String Int
  | Indexed AsmIndexedOperand

derive instance eqAsmOperand :: Eq AsmOperand
derive instance ordAsmOperand :: Ord AsmOperand

data AsmUnaryOperator = Neg | Not | Shr

derive instance eqAsmUnaryOperator :: Eq AsmUnaryOperator
derive instance ordAsmUnaryOperator :: Ord AsmUnaryOperator

data AsmBinaryOperator
  = Add
  | Sub
  | Mult
  | DivDouble
  | And
  | Or
  | Xor
  | Shl
  | ShrBinop

derive instance eqAsmBinaryOperator :: Eq AsmBinaryOperator
derive instance ordAsmBinaryOperator :: Ord AsmBinaryOperator

data AsmCondCode = E | NE | G | GE | L | LE | A | AE | B | BE

derive instance eqAsmCondCode :: Eq AsmCondCode
derive instance ordAsmCondCode :: Ord AsmCondCode

type AsmByteArrayInfo = { size :: Int, alignment :: Int }

data AsmType
  = Byte
  | Longword
  | Quadword
  | AsmDouble
  | ByteArray AsmByteArrayInfo

derive instance eqAsmType :: Eq AsmType
derive instance ordAsmType :: Ord AsmType

type AsmMovsxInfo =
  { src_type :: AsmType
  , dst_type :: AsmType
  , src :: AsmOperand
  , dst :: AsmOperand
  }

type AsmMovZeroExtendInfo =
  { src_type :: AsmType
  , dst_type :: AsmType
  , src :: AsmOperand
  , dst :: AsmOperand
  }

type AsmBinaryInfo =
  { op :: AsmBinaryOperator
  , t :: AsmType
  , src :: AsmOperand
  , dst :: AsmOperand
  }

data AsmInstruction
  = Mov AsmType AsmOperand AsmOperand
  | Movsx AsmMovsxInfo
  | MovZeroExtend AsmMovZeroExtendInfo
  | Lea AsmOperand AsmOperand
  | Cvttsd2si AsmType AsmOperand AsmOperand
  | Cvtsi2sd AsmType AsmOperand AsmOperand
  | Unary AsmUnaryOperator AsmType AsmOperand
  | Binary AsmBinaryInfo
  | Cmp AsmType AsmOperand AsmOperand
  | Idiv AsmType AsmOperand
  | Div AsmType AsmOperand
  | Cdq AsmType
  | Jmp String
  | JmpCC AsmCondCode String
  | SetCC AsmCondCode AsmOperand
  | Label String
  | Push AsmOperand
  | Pop AsmReg
  | Call String
  | Ret

derive instance eqAsmInstruction :: Eq AsmInstruction
derive instance ordAsmInstruction :: Ord AsmInstruction

type AsmFunctionDef =
  { name :: String
  , isGlobal :: Boolean
  , instructions :: List AsmInstruction
  }

type AsmStaticVariableDef =
  { name :: String
  , alignment :: Int
  , isGlobal :: Boolean
  , init :: List StaticInit
  }

type AsmStaticConstantDef =
  { name :: String
  , alignment :: Int
  , init :: StaticInit
  }

data AsmTopLevel
  = Function AsmFunctionDef
  | StaticVariable AsmStaticVariableDef
  | StaticConstant AsmStaticConstantDef

derive instance eqAsmTopLevel :: Eq AsmTopLevel

data AsmProgram = Program (List AsmTopLevel)

derive instance eqAsmProgram :: Eq AsmProgram
