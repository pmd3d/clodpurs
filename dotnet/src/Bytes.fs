module Bytes

let ofString (s: string) : byte[] =
    System.Text.Encoding.Latin1.GetBytes(s)

let make (n: int) (c: char) : byte[] =
    Array.create n (byte c)

let length (b: byte[]) = b.Length

let cat (a: byte[]) (b: byte[]) = Array.append a b

let sub (b: byte[]) (offset: int) (len: int) =
    b.[offset .. offset + len - 1]

let getInt64Le (b: byte[]) (offset: int) =
    System.BitConverter.ToInt64(b, offset)

let getInt32Le (b: byte[]) (offset: int) =
    System.BitConverter.ToInt32(b, offset)

let getInt8 (b: byte[]) (offset: int) =
    sbyte b.[offset]