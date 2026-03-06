module Initializers

type StaticInit =
    | CharInit of sbyte
    | UCharInit of byte
    | IntInit of int32
    | LongInit of int64
    | UIntInit of uint32
    | ULongInit of uint64
    | DoubleInit of float
    (* zero out arbitrary number of bytes *)
    | ZeroInit of int
    | StringInit of string * bool (* flag indicates whether the string is null terminated *)
    | PointerInit of string (* pointer to static variable *)

let showStaticInit = function
    | CharInit c -> string c
    | UCharInit uc -> string uc
    | IntInit i -> string i
    | LongInit l -> string l + "l"
    | UIntInit u -> string u + "u"
    | ULongInit ul -> string ul + "ul"
    | DoubleInit dbl -> string dbl
    | ZeroInit i -> sprintf "zero[%d]" i
    | StringInit(s, b) ->
        "\"" + s + (if b then "\\0" else "") + "\""
    | PointerInit s -> sprintf "&%s" s

let ppStaticInit (fmt: System.IO.TextWriter) si =
    fmt.Write(showStaticInit si)

let zero tt t = TypeUtils.getSize tt t |> Result.map (fun size -> [ ZeroInit(int size) ])

let isZero = function
    | CharInit c -> c = 0y
    | IntInit i -> i = 0
    | LongInit l -> l = 0L
    | UCharInit c -> c = 0uy
    | UIntInit u -> u = 0u
    | ULongInit ul -> ul = 0UL
    (* NOTE: consider all doubles non-zero since we don't know if it's zero or
       negative zero *)
    | DoubleInit _ -> false
    | ZeroInit _ -> true
    | PointerInit _ | StringInit _ -> false