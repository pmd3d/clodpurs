module TypeUtils

open Types

let getType (e: Ast.TypedExp.Exp) = e.t
let setType e newType : Ast.TypedExp.Exp = { e = e; t = newType }

let rec getSize (tt: TypeTable.TypeTableMap) =
    function
    | Char | SChar | UChar -> Ok 1L
    | Int | UInt -> Ok 4L
    | Long | ULong | Double | Pointer _ -> Ok 8L
    | Array(elemType, size) -> getSize tt elemType |> Result.map (fun s -> size * s)
    | Structure tag -> TypeTable.find tag tt |> Result.map (fun sd -> int64 sd.size)
    | (FunType _ | Void) as t ->
        Error (CompilerError.InternalError ("type doesn't have size: " + show t))

let rec getAlignment (tt: TypeTable.TypeTableMap) =
    function
    | Char | SChar | UChar -> Ok 1
    | Int | UInt -> Ok 4
    | Long | ULong | Double | Pointer _ -> Ok 8
    | Array(elemType, _) -> getAlignment tt elemType
    | Structure tag -> TypeTable.find tag tt |> Result.map (fun sd -> sd.alignment)
    | (FunType _ | Void) as t ->
        Error (CompilerError.InternalError ("type doesn't have alignment: " + show t))

let isSigned =
    function
    | Int | Long | Char | SChar -> Ok true
    | UInt | ULong | Pointer _ | UChar -> Ok false
    | (Double | FunType _ | Array _ | Void | Structure _) as t ->
        Error (CompilerError.InternalError
            ("signedness doesn't make sense for non-integral type "
             + show t))

let isPointer = function Pointer _ -> true | _ -> false

let isInteger =
    function
    | Char | UChar | SChar | Int | UInt | Long | ULong -> true
    | Double | Array _ | Pointer _ | FunType _ | Void | Structure _ -> false

let isArray = function Array _ -> true | _ -> false
let isCharacter = function Char | SChar | UChar -> true | _ -> false

let isArithmetic =
    function
    | Int | UInt | Long | ULong | Char | UChar | SChar | Double -> true
    | FunType _ | Pointer _ | Array _ | Void | Structure _ -> false

let isScalar =
    function
    | Array _ | Void | FunType _ | Structure _ -> false
    | Int | UInt | Long | ULong | Char | UChar | SChar | Double | Pointer _ ->
        true

let isComplete (tt: TypeTable.TypeTableMap) =
    function
    | Void -> false
    | Structure tag -> TypeTable.mem tag tt
    | _ -> true

let isCompletePointer tt = function Pointer t -> isComplete tt t | _ -> false
