module TestParser

open System
open Xunit
open Parse
open Ast
open TokStream

let printConst c = sprintf "%A" c

let unwrap r =
    match r with
    | Ok v -> v
    | Error msg -> failwithf "Unexpected parse error: %s" msg

[<Fact>]
let ``signed long constant`` () =
    let resultString =
        [ Tokens.ConstLong 4611686018427387904I ]
        |> TokStream.ofList
        |> Parse.parseConst
        |> unwrap
        |> fst
        |> printConst

    Assert.Equal("ConstLong 4611686018427387904L", resultString)

[<Fact>]
let ``unsigned int constant`` () =
    let resultString =
        [ Tokens.ConstUInt 4294967291I ]
        |> TokStream.ofList
        |> Parse.parseConst
        |> unwrap
        |> fst
        |> printConst

    Assert.Equal("ConstUInt 4294967291u", resultString)

[<Fact>]
let ``unsigned long constant`` () =
    let resultString =
        [ Tokens.ConstULong 18446744073709551611I ]
        |> TokStream.ofList
        |> Parse.parseConst
        |> unwrap
        |> fst
        |> printConst

    Assert.Equal("ConstULong 18446744073709551611UL", resultString)

[<Fact>]
let ``expression`` () =
    let result =
        [ Tokens.ConstInt 100; Tokens.Semicolon ]
        |> TokStream.ofList
        |> Parse.parseExp 40
        |> unwrap
        |> fst

    Assert.Equal(Ast.UntypedExp.Constant (Const.ConstInt 100), result)

[<Fact>]
let ``statement`` () =
    let result =
        [ Tokens.KWReturn; Tokens.ConstInt 4; Tokens.Semicolon ]
        |> TokStream.ofList
        |> Parse.parseStatement
        |> unwrap
        |> fst
    Assert.Equal(Ast.Untyped.Return (Some (Ast.UntypedExp.Constant (Const.ConstInt 4))), result)

[<Fact>]
let ``error`` () =
    let result = [ Tokens.KWInt ] |> Parse.parse
    match result with
    | Error _ -> ()
    | Ok _ -> failwith "Expected parse error but got Ok"
