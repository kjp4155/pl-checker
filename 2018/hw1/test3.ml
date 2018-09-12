(* Exercise 3. parenize *)
open Ex3
open Testlib

module TestEx3: TestEx =
  struct
    type testcase =
      | PARENIZE of tourna * string * string

    let testcases: testcase list =
      [ PARENIZE((NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil) ),
          "NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil)",
          "((Korea Portugal) Brazil)");
        PARENIZE((LEAF Portugal),
          "LEAF Portugal",
          "Portugal");
        PARENIZE((NODE(LEAF Korea, LEAF Portugal) ),
          "NODE(LEAF Korea, LEAF Portugal)",
          "(Korea Portugal)");
        PARENIZE(( NODE(NODE(LEAF Italy, LEAF England), LEAF Poland) ),
          "NODE(NODE(LEAF Italy, LEAF England), LEAF Poland)",
          "((Italy England) Poland)");
        PARENIZE(( NODE(NODE(LEAF Sweden, LEAF Portugal), LEAF Nigeria) ),
          "NODE(NODE(LEAF Sweden, LEAF Portugal), LEAF Nigeria)",
          "((Sweden Portugal) Nigeria)");
        PARENIZE(( NODE(NODE(NODE(NODE(LEAF Argentina, LEAF Germany), NODE(LEAF Usa, LEAF Brazil)), NODE(NODE(LEAF Cameroon, LEAF England), NODE(LEAF Portugal, LEAF Norway))), NODE(NODE(NODE(LEAF Nigeria, LEAF Italy), NODE(LEAF Poland, LEAF Japan)), NODE(NODE(LEAF France, LEAF Korea), LEAF Sweden))) ),
          "NODE(NODE(NODE(NODE(LEAF Argentina, LEAF Germany), NODE(LEAF Usa, LEAF Brazil)), NODE(NODE(LEAF Cameroon, LEAF England), NODE(LEAF Portugal, LEAF Norway))), NODE(NODE(NODE(LEAF Nigeria, LEAF Italy), NODE(LEAF Poland, LEAF Japan)), NODE(NODE(LEAF France, LEAF Korea), LEAF Sweden)))",
          "((((Argentina Germany) (Usa Brazil)) ((Cameroon England) (Portugal Norway))) (((Nigeria Italy) (Poland Japan)) ((France Korea) Sweden)))");
        PARENIZE(( NODE(NODE(NODE(NODE(LEAF Norway, LEAF Portugal), NODE(LEAF Korea, LEAF Sweden)), NODE(NODE(LEAF England, LEAF Japan), NODE(LEAF Nigeria, LEAF Poland))), NODE(NODE(NODE(LEAF France, LEAF Germany), NODE(LEAF Cameroon, LEAF Usa)), NODE(NODE(LEAF Argentina, LEAF Italy), LEAF Brazil))) ),
          "NODE(NODE(NODE(NODE(LEAF Norway, LEAF Portugal), NODE(LEAF Korea, LEAF Sweden)), NODE(NODE(LEAF England, LEAF Japan), NODE(LEAF Nigeria, LEAF Poland))), NODE(NODE(NODE(LEAF France, LEAF Germany), NODE(LEAF Cameroon, LEAF Usa)), NODE(NODE(LEAF Argentina, LEAF Italy), LEAF Brazil)))",
          "((((Norway Portugal) (Korea Sweden)) ((England Japan) (Nigeria Poland))) (((France Germany) (Cameroon Usa)) ((Argentina Italy) Brazil)))");
        PARENIZE(( NODE(LEAF France, NODE(LEAF Poland, NODE(LEAF Japan, NODE(LEAF England, NODE(LEAF Nigeria, NODE(LEAF Portugal, NODE(LEAF Argentina, NODE(LEAF Germany, NODE(LEAF Brazil, NODE(LEAF Cameroon, NODE(LEAF Korea, NODE(LEAF Sweden, NODE(LEAF Usa, NODE(LEAF Italy, LEAF Norway)))))))))))))) ),
          "NODE(LEAF France, NODE(LEAF Poland, NODE(LEAF Japan, NODE(LEAF England, NODE(LEAF Nigeria, NODE(LEAF Portugal, NODE(LEAF Argentina, NODE(LEAF Germany, NODE(LEAF Brazil, NODE(LEAF Cameroon, NODE(LEAF Korea, NODE(LEAF Sweden, NODE(LEAF Usa, NODE(LEAF Italy, LEAF Norway))))))))))))))",
          "(France (Poland (Japan (England (Nigeria (Portugal (Argentina (Germany (Brazil (Cameroon (Korea (Sweden (Usa (Italy Norway))))))))))))))");
        PARENIZE(( NODE(LEAF Portugal, NODE(LEAF Brazil, NODE(LEAF Sweden, NODE(LEAF Italy, NODE(LEAF Argentina, NODE(LEAF Poland, NODE(LEAF Cameroon, NODE(LEAF Usa, NODE(LEAF Nigeria, NODE(LEAF Germany, NODE(LEAF England, NODE(LEAF Japan, NODE(LEAF Norway, NODE(LEAF Korea, LEAF France)))))))))))))) ),
          "NODE(LEAF Portugal, NODE(LEAF Brazil, NODE(LEAF Sweden, NODE(LEAF Italy, NODE(LEAF Argentina, NODE(LEAF Poland, NODE(LEAF Cameroon, NODE(LEAF Usa, NODE(LEAF Nigeria, NODE(LEAF Germany, NODE(LEAF England, NODE(LEAF Japan, NODE(LEAF Norway, NODE(LEAF Korea, LEAF France))))))))))))))",
          "(Portugal (Brazil (Sweden (Italy (Argentina (Poland (Cameroon (Usa (Nigeria (Germany (England (Japan (Norway (Korea France))))))))))))))");      
        PARENIZE(( NODE(LEAF Usa, NODE(LEAF Norway, NODE(LEAF England, NODE(LEAF Italy, NODE(LEAF Nigeria, NODE(LEAF Argentina, NODE(LEAF Korea, NODE(LEAF Brazil, NODE(LEAF Sweden, NODE(LEAF Portugal, NODE(LEAF Germany, NODE(LEAF Japan, NODE(LEAF France, NODE(LEAF Poland, LEAF Cameroon)))))))))))))) ),
          "NODE(LEAF Usa, NODE(LEAF Norway, NODE(LEAF England, NODE(LEAF Italy, NODE(LEAF Nigeria, NODE(LEAF Argentina, NODE(LEAF Korea, NODE(LEAF Brazil, NODE(LEAF Sweden, NODE(LEAF Portugal, NODE(LEAF Germany, NODE(LEAF Japan, NODE(LEAF France, NODE(LEAF Poland, LEAF Cameroon))))))))))))))",
          "(Usa (Norway (England (Italy (Nigeria (Argentina (Korea (Brazil (Sweden (Portugal (Germany (Japan (France (Poland Cameroon))))))))))))))");
        PARENIZE(( NODE(LEAF Norway, NODE(NODE(LEAF Cameroon, LEAF Poland), LEAF Sweden))),
          "NODE(LEAF Norway, NODE(NODE(LEAF Cameroon, LEAF Poland), LEAF Sweden))",
          "(Norway ((Cameroon Poland) Sweden))");
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
