(* Exercise 2. sumprod *)
open Ex2
open Testlib

module TestEx2: TestEx =
  struct
    type testcase =
      | SUMPROD of (int * int -> float) * int * int * string * float

    let testcases: testcase list =
      [ SUMPROD ((fun (x,y) -> float_of_int x +. float_of_int y), 1, 1, "x -> x", 2.0)
      ; SUMPROD ((fun (x,y) -> float_of_int x *. float_of_int y), 1, 1, "x -> x*x", 1.0)
      ;
      ]

    let runner (tc: testcase): bool =
      match tc with
      | SUMPROD (f, n, k, fs, ans) -> sumprod(f,n,k) = ans

    let string_of_tc (tc: testcase): string * string * string =
      match tc with
      | SUMPROD (f, n, k, fs, ans) ->
          ( Printf.sprintf "sumprod(%s, %d, %d)" fs n k
          , string_of_float ans
          , string_of_float (sumprod(f,n,k))
          )
  end

open TestEx2
let _ = wrapper testcases runner string_of_tc
