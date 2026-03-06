module Tacky

type TackyUnaryOperator = Complement | Negate | Not

type TackyBinaryOperator =
    | Add
    | Subtract
    | Multiply
    | Divide
    | Mod
    | Equal
    | NotEqual
    | LessThan
    | LessOrEqual
    | GreaterThan
    | GreaterOrEqual

// we need a custom comparison function for constants to make sure that 0.0 and
// -0.0 don't compare equal
let constCompare a b =
    match (a, b) with
    | Const.ConstDouble d1, Const.ConstDouble d2 when d1 = d2 ->
        compare (System.Math.CopySign(1.0, d1)) (System.Math.CopySign(1.0, d2))
    | _ -> compare a b

type TackyVal =
    | Constant of Const.ConstValue
    | Var of string

let showTackyVal = function Constant c -> Const.show c | Var v -> v
let ppTackyVal (fmt: System.IO.TextWriter) v = fmt.Write(showTackyVal v)

// TODO maybe this should be in a separate module?
let typeOfVal st = function
    // note: this reports the type of ConstChar as SChar instead of Char, doesn't
    // matter in this context
    | Constant c -> Ok (Const.typeOfConst c)
    | Var v -> Symbols.get v st |> Result.map (fun entry -> entry.symType)

type TackySrcDst = { src: TackyVal; dst: TackyVal }

type TackyUnaryInfo = { op: TackyUnaryOperator; src: TackyVal; dst: TackyVal }

type TackyBinaryInfo = {
    op: TackyBinaryOperator
    src1: TackyVal
    src2: TackyVal
    dst: TackyVal
}

type TackyAddPtrInfo = {
    ptr: TackyVal
    index: TackyVal
    scale: int
    dst: TackyVal
}

type TackyCopyToOffsetInfo = { src: TackyVal; dst: string; offset: int }

type TackyCopyFromOffsetInfo = { src: string; offset: int; dst: TackyVal }

type TackyFunCallInfo = { f: string; args: TackyVal list; dst: TackyVal option }

type TackyLoadInfo = { src_ptr: TackyVal; dst: TackyVal }
type TackyStoreInfo = { src: TackyVal; dst_ptr: TackyVal }

type TackyInstruction =
    | Return of TackyVal option
    | SignExtend of TackySrcDst
    | ZeroExtend of TackySrcDst
    | DoubleToInt of TackySrcDst
    | IntToDouble of TackySrcDst
    | DoubleToUInt of TackySrcDst
    | UIntToDouble of TackySrcDst
    | Truncate of TackySrcDst
    | Unary of TackyUnaryInfo
    | Binary of TackyBinaryInfo
    | Copy of TackySrcDst
    | GetAddress of TackySrcDst
    | Load of TackyLoadInfo
    | Store of TackyStoreInfo
    | AddPtr of TackyAddPtrInfo
    | CopyToOffset of TackyCopyToOffsetInfo
    | CopyFromOffset of TackyCopyFromOffsetInfo
    | Jump of string
    | JumpIfZero of TackyVal * string
    | JumpIfNotZero of TackyVal * string
    | Label of string
    | FunCall of TackyFunCallInfo

type TackyFunctionDef = {
    name: string
    isGlobal: bool
    paramList: string list
    body: TackyInstruction list
}

type TackyStaticVariableDef = {
    name: string
    t: Types.CType
    isGlobal: bool
    init: Initializers.StaticInit list
}

type TackyStaticConstantDef = {
    name: string
    t: Types.CType
    init: Initializers.StaticInit
}

type TackyTopLevel =
    | Function of TackyFunctionDef
    | StaticVariable of TackyStaticVariableDef
    | StaticConstant of TackyStaticConstantDef

type TackyProgram = Program of TackyTopLevel list
