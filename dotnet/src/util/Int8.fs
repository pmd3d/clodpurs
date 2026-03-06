module Int8

(* Represent signed chars internally with the int32 type *)
type Int8Value = int32

let equal (a: Int8Value) (b: Int8Value) = a = b
let compare (a: Int8Value) (b: Int8Value) = compare a b

let show (i8: Int8Value) = i8.ToString()

let zero: Int8Value = 0

(* internal function to sign-or-zero-extend into upper bytes *)
let resetUpperBytes (x: Int8Value) =
    if x &&& 128 = 0 then
        (* result is positive, zero out upper bits *)
        let bitmask = 0x000000ff
        x &&& bitmask
    else
        (* result is negative, set upper bits to 1 *)
        let bitmask = 0xffffff00 |> int32
        x ||| bitmask

let ofInt (i: int) : Int8Value = resetUpperBytes (int32 i)
let toInt (x: Int8Value) : int = int x
let ofInt64 (i: int64) : Int8Value = resetUpperBytes (int32 i)
let toInt64 (x: Int8Value) : int64 = int64 x
let toString (x: Int8Value) : string = x.ToString()