module TestUtil

open Xunit
open ListUtil

[<Fact>]
let ``empty_0`` () =
    Assert.True(takeDrop 0 [] = ([], []))

[<Fact>]
let ``empty_n`` () =
    Assert.True(takeDrop 10 [] = ([], []))

[<Fact>]
let ``one`` () =
    Assert.True(takeDrop 1 [ 'a'; 'b'; 'c' ] = ([ 'a' ], [ 'b'; 'c' ]))

[<Fact>]
let ``three`` () =
    Assert.True(takeDrop 3 [ 1; 2; 3; 4; 5; 6; 7 ] = ([ 1; 2; 3 ], [ 4; 5; 6; 7 ]))

[<Fact>]
let ``n_gt_len`` () =
    Assert.True(takeDrop 10 [ 'a'; 'b'; 'c' ] = ([ 'a'; 'b'; 'c' ], []))

[<Fact>]
let ``all`` () =
    Assert.True(takeDrop 3 [ 'a'; 'b'; 'c' ] = ([ 'a'; 'b'; 'c' ], []))

[<Fact>]
let ``take_zero`` () =
    Assert.True(take 0 [ 1; 2; 3; 4; 5 ] = [])

[<Fact>]
let ``take_all`` () =
    Assert.True(take 10 [ 1; 2; 3; 4; 5 ] = [ 1; 2; 3; 4; 5 ])

[<Fact>]
let ``take_some`` () =
    Assert.True(take 3 [ 1; 2; 3; 4; 5 ] = [ 1; 2; 3 ])
