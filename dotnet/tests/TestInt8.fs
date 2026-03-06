module TestInt8

open Int8
open Xunit

let print_int8 i8 = sprintf "%A" i8

[<Fact>]
let ``int to int8`` () =
    let resultString = Int8.ofInt 100 |> print_int8 
    Assert.Equal("100", resultString)

[<Fact>]
let ``wrapped int to int8`` () =
    let resultString = Int8.ofInt 128  |> print_int8 
    Assert.Equal("-128", resultString)

[<Fact>]
let ``int64 to int8`` () =
    let resultString = Int8.ofInt64 (-110L) |> print_int8 
    Assert.Equal("-110", resultString)

[<Fact>]
let ``wrapped int64 to int8`` () =
    let resultString = Int8.ofInt64 1239235325L |> print_int8
    Assert.Equal("-3", resultString)

[<Fact>]
let ``compare int8 values`` () =
    let twelve = Int8.ofInt 268 in
    let fourteen = Int8.ofInt(-4082) in
    Assert.Equal(compare twelve fourteen, -1)

