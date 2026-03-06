module ConstConvert

module C = Const
module T = Types
module Ast = Ast.Typed

(** Convert constant to an int64. If constant is smaller than int64 it will be
    zero- or sign-extended to preserve value; if it's the same size we preserve
    its representation. *)
let constToInt64 = function
    | C.ConstChar c -> int64 c
    | C.ConstUChar uc -> int64 uc
    | C.ConstInt i -> int64 i
    | C.ConstUInt ui -> int64 ui
    | C.ConstLong l -> l
    | C.ConstULong ul -> int64 ul
    | C.ConstDouble d -> int64 d

(** Convert int64 to a constant. Preserve the value if possible and wrap modulo
    the size of the target type otherwise. *)
let constOfInt64 v = function
    | T.Char | T.SChar -> Ok (C.ConstChar(sbyte v))
    | T.UChar -> Ok (C.ConstUChar(byte v))
    | T.Int -> Ok (C.ConstInt(int32 v))
    | T.Long -> Ok (C.ConstLong v)
    | T.UInt -> Ok (C.ConstUInt(uint32 v))
    | T.ULong | T.Pointer _ -> Ok (C.ConstULong(uint64 v))
    | T.Double -> Ok (C.ConstDouble(float v))
    | (T.FunType _ | T.Array _ | T.Void | T.Structure _) as t ->
        Error (CompilerError.InternalError
            ("can't convert constant to non_scalar type "
             + Types.show t))

let uint64ToDouble (ul: uint64) =
    if ul <= uint64 System.Int64.MaxValue then
        float (int64 ul)
    else
        let half = ul >>> 1
        let lsb = ul &&& 1UL
        let halfDouble = float (int64 (half ||| lsb))
        halfDouble + halfDouble

let constConvert target_type c =
    if C.typeOfConst c = target_type then Ok c
    else
        match (target_type, c) with
        (* Convert to/from double directly to avoid precision loss
           going through the int64 roundtrip *)
        | T.Double, C.ConstULong ul ->
            Ok (C.ConstDouble(uint64ToDouble ul))
        | T.Double, _ ->
            Ok (C.ConstDouble(float (constToInt64 c)))
        | T.ULong, C.ConstDouble d ->
            Ok (C.ConstULong(uint64 d))
        | _, C.ConstDouble d ->
            constOfInt64 (int64 d) target_type
        | _ ->
            (* Convert c to int64, then to target type, to avoid exponential
               explosion of different cases. Conversion to int64 preserves value
               (except when converting from out-of-range ulong, where it preserves
               representation). Conversion from int64 to const wraps modulo const
               size. *)
            let as_int64 = constToInt64 c
            constOfInt64 as_int64 target_type