(* Exercise 4. eval *)
open Ex4
open Testlib

module TestEx4: TestEx =
  struct
    type testcase =
      | EVAL of formula * string * bool
      

    let testcases: testcase list =
      [ 
        EVAL (
          LESS((PLUS(NUM 3,NUM 5)) , (PLUS(NUM 5,NUM 10))),
          "(3+5) < (5+10)",
          true
        );
        EVAL (
          LESS((MINUS(NUM 3,NUM 5)) , (MINUS(NUM 5,NUM 10))),
          "(3-5) < (5-10)",
          false
        );
        EVAL (
          ANDALSO(TRUE, FALSE),
          "T && F",
          false
        );
        EVAL (
          ORELSE(TRUE, FALSE),
          "T || F",
          true
        );
      ]

    let runner (tc: testcase): bool =
      match tc with
      | EVAL (f,s,ans) -> eval f = ans
      
    let string_of_tc (tc: testcase): string * string * string =
      match tc with
      | EVAL (f, s, ans) ->
          ( Printf.sprintf "eval(%s)" s
          , string_of_bool ans
          , string_of_bool (eval f)
          )

end

open TestEx4
let _ = wrapper testcases runner string_of_tc
