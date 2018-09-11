(* Exercise 3. parenize *)
open Ex3
open Testlib

module TestEx3: TestEx =
  struct
    type testcase =
      | PARENIZE of tourna * string * string

    let testcases: testcase list =
      [ PARENIZE ((NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil)),
        "(NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil))",
        "((Korea Portugal) Brazil)")
      ; 
      ]

    let runner (tc: testcase): bool =
      match tc with
      | PARENIZE (t, ts, ans) -> parenize(t) = ans

    let string_of_tc (tc: testcase): string * string * string =
      match tc with
      | PARENIZE (t, ts, ans) ->
          ( Printf.sprintf "parenize(%s)" ts
          , ans
          , parenize(t)
          )
  end

open TestEx3
let _ = wrapper testcases runner string_of_tc
