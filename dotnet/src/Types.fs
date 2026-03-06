module Types

type CType =
    | Char
    | SChar
    | UChar
    | Int
    | Long
    | UInt
    | ULong
    | Double
    | Pointer of CType
    | Void
    | Array of elemType: CType * size: int64
    | FunType of paramTypes: CType list * retType: CType
    | Structure of string (* tag *)

let rec show (typ: CType) : string =
    match typ with
    | Char -> "Char"
    | SChar -> "SChar"
    | UChar -> "UChar"
    | Int -> "Int"
    | Long -> "Long"
    | UInt -> "UInt"
    | ULong -> "ULong"
    | Double -> "Double"
    | Pointer inner -> sprintf "%s*" (show inner)
    | Void -> "Void"
    | Array(elemType, size) ->
        sprintf "(%s, %s)" (show elemType) (string size)
    | FunType(paramTypes, retType) ->
        let paramStr =
            paramTypes
            |> List.map show
            |> String.concat "; "
        sprintf "(FunType (param_types = [%s], ret_type = %s))"
            paramStr
            (show retType)
    | Structure tag -> sprintf "(Structure %s)" tag

let pp (out: System.IO.TextWriter) (t: CType) = out.Write(show t)