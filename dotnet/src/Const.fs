module Const

[<CustomEquality; CustomComparison>]
type ConstValue =
    | ConstChar of sbyte
    | ConstUChar of byte
    | ConstInt of int32
    | ConstLong of int64
    | ConstUInt of uint32
    | ConstULong of uint64
    | ConstDouble of float

    override this.Equals(obj) =
        match obj with
        | :? ConstValue as other ->
            match (this, other) with
            | ConstChar a, ConstChar b -> a = b
            | ConstUChar a, ConstUChar b -> a = b
            | ConstInt a, ConstInt b -> a = b
            | ConstLong a, ConstLong b -> a = b
            | ConstUInt a, ConstUInt b -> a = b
            | ConstULong a, ConstULong b -> a = b
            | ConstDouble a, ConstDouble b -> a = b
            | _ -> false
        | _ -> false

    override this.GetHashCode() =
        match this with
        | ConstChar c -> hash (0, c)
        | ConstUChar c -> hash (1, c)
        | ConstInt i -> hash (2, i)
        | ConstLong l -> hash (3, l)
        | ConstUInt u -> hash (4, u)
        | ConstULong ul -> hash (5, ul)
        | ConstDouble d -> hash (6, d)

    interface System.IComparable with
        member this.CompareTo(obj) =
            let tag =
                function
                | ConstChar _ -> 0
                | ConstUChar _ -> 1
                | ConstInt _ -> 2
                | ConstLong _ -> 3
                | ConstUInt _ -> 4
                | ConstULong _ -> 5
                | ConstDouble _ -> 6
            match obj with
            | :? ConstValue as other ->
                match (this, other) with
                | ConstChar a, ConstChar b -> compare a b
                | ConstUChar a, ConstUChar b -> compare a b
                | ConstInt a, ConstInt b -> compare a b
                | ConstLong a, ConstLong b -> compare a b
                | ConstUInt a, ConstUInt b -> compare a b
                | ConstULong a, ConstULong b -> compare a b
                | ConstDouble a, ConstDouble b -> compare a b
                | _ -> compare (tag this) (tag other)
            | _ ->
                invalidArg "obj" "Cannot compare values of different types"

(* print functions for debugging *)
let show = function
    | ConstChar c -> string c
    | ConstUChar c -> string c
    | ConstInt i -> string i
    | ConstLong l -> string l + "L"
    | ConstUInt u -> string u + "U"
    | ConstULong ul -> string ul + "UL"
    | ConstDouble d -> sprintf "%.54g" d

let pp (fmt: System.IO.TextWriter) cnst =
    fmt.Write(show cnst)

let intZero = ConstInt 0
let intOne = ConstInt 1

let typeOfConst = function
    | ConstChar _ -> Types.SChar
    | ConstUChar _ -> Types.UChar
    | ConstInt _ -> Types.Int
    | ConstLong _ -> Types.Long
    | ConstUInt _ -> Types.UInt
    | ConstULong _ -> Types.ULong
    | ConstDouble _ -> Types.Double