(* Exercise 1. eval formula *)
open Ex1
open Testlib
open Printf

module TestEx1: TestEx =
  struct
    type testcase =
      | CALCULATE of exp * string * float

    let testcases =
      [ 
        CALCULATE(
          SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1)),
          "SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1))",
          375.0
        );
        CALCULATE(
          INTEGRAL(REAL 1.0, REAL 10.0, SUB(MUL(X, X), INT 1)),
          "INTEGRAL(REAL 1.0, REAL 10.0, SUB(MUL(X, X), INT 1))",
          319.065
        );
        CALCULATE(
          INTEGRAL(REAL 1.0, REAL 0.0, SUB(MUL(X, X), INT 0)),
          "INTEGRAL(REAL 1.0, REAL 10.0, SUB(MUL(X, X), INT 1))",
          -0.285
        );
      ]

    let runner tc =
      match tc with
      | CALCULATE (f, fs, ans) -> abs_float ((calculate f) -. ans) < 0.00000001

    let string_of_tc tc =
      match tc with
      | CALCULATE (f, fs, ans) -> 
          ( fs  
          , Printf.sprintf "%.10f" ans
          , Printf.sprintf "%.10f" (calculate f)
          )
  end

open TestEx1
let _ = wrapper testcases runner string_of_tc
