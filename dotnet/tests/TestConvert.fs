module TestConvert

open Xunit
open System

let print_const c =
  match c with
    | Const.ConstDouble d ->
      sprintf "ConstDouble %.20g" d
    | _ -> sprintf "%A" c

let convert_and_print target_type c =
  let converted = ConstConvert.constConvert target_type c in
  print_const converted

[<Fact>]
let ``preserve int value`` () =
    let i = Const.ConstInt 1000l in
    Assert.Equal("ConstLong 1000L", convert_and_print Types.Long i)

[<Fact>]
let ``preserve negative int value`` () =
    let i = Const.ConstInt (-1000l) in
    Assert.Equal("ConstLong -1000L", convert_and_print Types.Long i)

[<Fact>]
let ``preserve long value`` () =
    let l = Const.ConstLong 200000L in
    Assert.Equal("ConstInt 200000", convert_and_print Types.Int l)

[<Fact>]
let ``truncate positive long`` () =
    (* l is 2^52 + 5 *)
    let l = Const.ConstLong 4503599627370501L in
    Assert.Equal("ConstInt 5", convert_and_print Types.Int l)

[<Fact>]
let ``truncate positive long to negative`` () =
    (* l is 2^52 - 5 *)
    let l = Const.ConstLong 4503599627370491L in
    Assert.Equal("ConstInt -5", convert_and_print Types.Int l)

[<Fact>]
let ``truncate negative long to zero`` () =
    (* l is -2^33 *)
    let l = Const.ConstLong (-8589934592L) in
    Assert.Equal("ConstInt 0", convert_and_print Types.Int l)

[<Fact>]
let ``truncate negative long to negative`` () =
    (* l is -2^33 - 100 *)
    let l = Const.ConstLong (-8589934692L) in
    Assert.Equal("ConstInt -100", convert_and_print Types.Int l)

[<Fact>]
let ``trivial uint to int`` () =
    let ui = Const.ConstUInt 100u in
    Assert.Equal("ConstInt 100", convert_and_print Types.Int ui)

[<Fact>]
let ``wrapping uint to int`` () =
    let ui = Const.ConstUInt 4294967200u in
    Assert.Equal("ConstInt -96", convert_and_print Types.Int ui)

[<Fact>]
let ``trivial int to uint`` () =
    let i = Const.ConstInt 1000l in
    Assert.Equal("ConstUInt 1000u", convert_and_print Types.UInt i)

[<Fact>]
let ``wrapping int to uint`` () =
    let i = Const.ConstInt (-1000l) in
    Assert.Equal("ConstUInt 4294966296u", convert_and_print Types.UInt i)

[<Fact>]
let ``int to ulong`` () =
  let i = Const.ConstInt (-10l) in
  Assert.Equal("ConstULong 18446744073709551606UL", convert_and_print Types.ULong i)

[<Fact>]
let ``uint to long`` () =
  let ui = Const.ConstUInt 4294967200u in
  Assert.Equal("ConstLong 4294967200L", convert_and_print Types.Long ui)

[<Fact>]
let ``long to uint`` () =
  let l = Const.ConstLong (-9223372036854774574L) in
  Assert.Equal("ConstUInt 1234u", convert_and_print Types.UInt l)

[<Fact>]
let ``ulong to int`` () =
  let ul = Const.ConstULong 4294967200UL in
  Assert.Equal("ConstInt -96", convert_and_print Types.Int ul)

[<Fact>]
let ``ulong to uint`` () =
  let ul = Const.ConstULong 1152921506754330624UL in
  Assert.Equal("ConstUInt 2147483648u", convert_and_print Types.UInt ul)

[<Fact>]
let ``double to long`` () =
  let d = Const.ConstDouble 2148429099.3 in
  Assert.Equal("ConstLong 2148429099L", convert_and_print Types.Long d)

[<Fact>]
let ``double to int`` () =
  let d = Const.ConstDouble (-200000.9999) in
  Assert.Equal("ConstLong -200000L", convert_and_print Types.Long d)

[<Fact>]
let ``double to uint`` () =
  let d = Const.ConstDouble 2147483750.5 in
  Assert.Equal("ConstUInt 2147483750u", convert_and_print Types.UInt d)

[<Fact>]
let ``double to ulong`` () =
  let d = Const.ConstDouble 3458764513821589504.0 in
  Assert.Equal("ConstULong 3458764513821589504UL", convert_and_print Types.ULong d)

[<Fact>]
let ``int to double`` () =
  let i = Const.ConstInt (-1000l) in
  Assert.Equal("ConstDouble -1000", convert_and_print Types.Double i)

[<Fact>]
let ``long to double`` () =
  let l = Const.ConstLong (-9007199254751227L) in
  Assert.Equal("ConstDouble -9007199254751228", convert_and_print Types.Double l)

[<Fact>]
let ``uint to double`` () =
  let ui = Const.ConstUInt 4294967200u in
  Assert.Equal("ConstDouble 4294967200", convert_and_print Types.Double ui)

[<Fact>]
let ``ulong to double`` () =
  let ul = Const.ConstULong 138512825844UL in
  Assert.Equal("ConstDouble 138512825844", convert_and_print Types.Double ul)

[<Fact>]
let ``ulong to double inexact`` () =
  let ul = Const.ConstULong 10223372036854775816UL in
  Assert.Equal("ConstDouble 10223372036854775808", convert_and_print Types.Double ul)

[<Fact>]
let ``ulong to double round to odd`` () =
  let ul = Const.ConstULong 9223372036854776832UL in
  Assert.Equal("ConstDouble 9223372036854775808", convert_and_print Types.Double ul)

[<Fact>]
let ``ulong to double above halfway`` () =
    let ul = Const.ConstULong 9223372036854776833UL in
    Assert.Equal("ConstDouble 9223372036854777856", convert_and_print Types.Double ul)

[<Fact>]
let ``signed char to long`` () =
  let c = Const.ConstChar -10y in
  Assert.Equal("ConstLong -10L", convert_and_print Types.Long c)

[<Fact>]
let ``signed char to ulong`` () =
  let c = Const.ConstChar -10y in
  Assert.Equal("ConstULong 18446744073709551606UL", convert_and_print Types.ULong c)

[<Fact>]
let ``unsigned char to int`` () =
  let c = Const.ConstUChar 255uy in
  Assert.Equal("ConstInt 255", convert_and_print Types.Int c)

[<Fact>]
let ``signed char to double`` () =
  let c = Const.ConstChar (-70y) in
  Assert.Equal("ConstDouble -70", convert_and_print Types.Double c)

[<Fact>]
let ``unsigned char to double`` () =
  let c = Const.ConstUChar 200uy in
  Assert.Equal("ConstDouble 200", convert_and_print Types.Double c)

[<Fact>]
let ``long to char`` () =
  let l = Const.ConstLong (-1000L) in
  Assert.Equal("ConstChar 24y", convert_and_print Types.SChar l)

[<Fact>]
let ``uint to char`` () =
  let ui = Const.ConstUInt 2147483858u in
  Assert.Equal("ConstChar -46y", convert_and_print Types.Char ui)

[<Fact>]
let ``ulong to uchar`` () =
  let ul = Const.ConstULong 18446744073709551606UL in
  Assert.Equal("ConstUChar 246uy", convert_and_print Types.UChar ul)

[<Fact>]
let ``int to uchar`` () =
  let i = Const.ConstInt 356l in
  Assert.Equal("ConstUChar 100uy", convert_and_print Types.UChar i)

[<Fact>]
let ``double to char`` () =
  let d = Const.ConstDouble (-100.8) in
  Assert.Equal("ConstChar -100y", convert_and_print Types.Char d)

[<Fact>]
let ``double to uchar`` () =
  let d = Const.ConstDouble 250.1234 in
  Assert.Equal("ConstUChar 250uy", convert_and_print Types.UChar d)