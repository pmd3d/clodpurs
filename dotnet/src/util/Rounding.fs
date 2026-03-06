module Rounding

(* We're assuming that n > 0 *)
let roundAwayFromZero n =
    function
    | x when x % n = 0 -> x
    | x when x < 0 ->
        (* when x is negative and n is positive, x mod n will be negative *)
        x - n - (x % n)
    | x -> x + n - (x % n)