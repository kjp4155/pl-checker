(* Exercise 2. sumprod *)
open Ex2
open Testlib

module TestEx2: TestEx =
  struct
    type testcase =
      | SUMPROD of (int * int -> float) * int * int * string * float

    let testcases: testcase list =
      [ SUMPROD ((fun (x,y) -> float_of_int x +. float_of_int y), 1, 1, "(x,y) -> x+y", 2.0)
      ; SUMPROD ((fun (x,y) -> float_of_int x *. float_of_int y), 1, 1, "(x,y) -> x*y", 1.0)
      ; SUMPROD ((fun (x,y) -> float_of_int x *. float_of_int y), 2, 5, "(x,y) -> x*y", 3960.0)
      ; SUMPROD ((fun (x,y) -> float_of_int x *. float_of_int y), 3, 5, "(x,y) -> x*y", 33120.0)
      ; SUMPROD ((fun (x,y) -> float_of_int x *. float_of_int y), 3, 1, "(x,y) -> x*y", 6.0)
      ; SUMPROD ((fun (x,y) -> float_of_int x *. float_of_int y), 0, 5, "(x,y) -> x*y", 0.0)
      ; SUMPROD ((fun (x,y) -> float_of_int x *. float_of_int y), 5, 0, "(x,y) -> x*y", 5.0)
      ; SUMPROD ((fun (x,y) -> float_of_int x *. float_of_int y), 0, 0, "(x,y) -> x*y", 0.0)
      ; SUMPROD ((fun (x,y) -> float_of_int x *. float_of_int y), -1, 1, "(x,y) -> x*y", 0.0)
      ; SUMPROD ((fun (x,y) -> float_of_int x *. float_of_int y), 3, -1, "(x,y) -> x*y", 3.0)
      ; SUMPROD ((fun (x,y) -> float_of_int x *. float_of_int y), -5, -3, "(x,y) -> x*y", 0.0)
      ; SUMPROD ((fun (x,y) -> float_of_int x *. float_of_int y), 5, 1, "(x,y) -> x*y", 15.0)
      ; SUMPROD ((fun (x,y) -> (float_of_int x *. float_of_int x) +. (float_of_int y *. float_of_int y))
        , 3, 5, "(x,y) -> x*x + y*y", 2334800.0)
      ; SUMPROD ((fun (x,y) -> (float_of_int x *. float_of_int x) -. (float_of_int y *. float_of_int y))
        , 3, 5, "(x,y) -> x*x - y*y", 0.0)
      ; SUMPROD ((fun (x,y) -> (float_of_int x *. float_of_int x) *. (float_of_int y *. float_of_int y))
        , 3, 5, "(x,y) -> x*x * y*y", 865065600.0)
      ; SUMPROD ((fun (x,y) -> (float_of_int x *. float_of_int x) -. float_of_int y)
        , 8, 8, "(x,y) -> x*x - y", 172348703573760.0)
      ; SUMPROD ((fun (i, j) -> ((float_of_int i) *. 10.) +. (float_of_int j)), 2, 7, "TA Example", 4573689120.0)
      ]

    let runner (tc: testcase): bool =
      match tc with
      | SUMPROD (f, n, k, fs, ans) -> abs_float (sumprod(f,n,k) -. ans) < 0.0000001

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
