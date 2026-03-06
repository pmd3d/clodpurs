module Assembly

type AsmReg =
    | AX
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

type AsmIndexedOperand = { baseReg: AsmReg; index: AsmReg; scale: int }

type AsmOperand =
    | Imm of int64
    | Reg of AsmReg
    | Pseudo of string
    | Memory of AsmReg * int
    | Data of string * int
    | PseudoMem of string * int
    | Indexed of AsmIndexedOperand

type AsmUnaryOperator = Neg | Not | Shr

type AsmBinaryOperator =
    | Add
    | Sub
    | Mult
    | DivDouble
    | And
    | Or
    | Xor
    | Shl
    | ShrBinop

type AsmCondCode = E | NE | G | GE | L | LE | A | AE | B | BE

type AsmByteArrayInfo = { size: int; alignment: int }

type AsmType =
    | Byte
    | Longword
    | Quadword
    | Double
    | ByteArray of AsmByteArrayInfo

type AsmMovsxInfo = {
    src_type: AsmType
    dst_type: AsmType
    src: AsmOperand
    dst: AsmOperand
}

type AsmMovZeroExtendInfo = {
    src_type: AsmType
    dst_type: AsmType
    src: AsmOperand
    dst: AsmOperand
}

type AsmBinaryInfo = {
    op: AsmBinaryOperator
    t: AsmType
    src: AsmOperand
    dst: AsmOperand
}

type AsmInstruction =
    | Mov of AsmType * AsmOperand * AsmOperand
    | Movsx of AsmMovsxInfo
    | MovZeroExtend of AsmMovZeroExtendInfo
    | Lea of AsmOperand * AsmOperand
    | Cvttsd2si of AsmType * AsmOperand * AsmOperand
    | Cvtsi2sd of AsmType * AsmOperand * AsmOperand
    | Unary of AsmUnaryOperator * AsmType * AsmOperand
    | Binary of AsmBinaryInfo
    | Cmp of AsmType * AsmOperand * AsmOperand
    | Idiv of AsmType * AsmOperand
    | Div of AsmType * AsmOperand
    | Cdq of AsmType
    | Jmp of string
    | JmpCC of AsmCondCode * string
    | SetCC of AsmCondCode * AsmOperand
    | Label of string
    | Push of AsmOperand
    | Pop of AsmReg
    | Call of string
    | Ret

type AsmFunctionDef = {
    name: string
    isGlobal: bool
    instructions: AsmInstruction list
}

type AsmStaticVariableDef = {
    name: string
    alignment: int
    isGlobal: bool
    init: Initializers.StaticInit list
}

type AsmStaticConstantDef = {
    name: string
    alignment: int
    init: Initializers.StaticInit
}

type AsmTopLevel =
    | Function of AsmFunctionDef
    | StaticVariable of AsmStaticVariableDef
    | StaticConstant of AsmStaticConstantDef

type AsmProgram = Program of AsmTopLevel list
