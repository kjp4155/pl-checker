(*
 * SNU 4190.310 Programming Languages
 * Main driver of homework "Exceptions are sugar"
 *)

let main () =
  let print_input = ref false in
  let print_desugar = ref false in
  let result_only = ref false in
  let src = ref "" in
  let _ =
    Arg.parse
      [ ("-pp", Arg.Set print_input, "Print input xexp program");
        ("-pdesug", Arg.Set print_desugar, "Print desugared xexp program");
        ("-resonly", Arg.Set result_only, "Print result output and errors only");
      ]
      (fun x -> src := x)
      "Usage: ./run [<options>] <xexp file>"
  in

  let lexbuf =
    Lexing.from_channel (if !src = "" then stdin else open_in !src)
  in
  let input_pgm = Parser.program Lexer.start lexbuf in
  if !print_input then (
    print_endline "== Input Program ==";
    Xexp.print input_pgm;
    print_newline ()
  );
  let desugared_pgm = Desugar.removeExn input_pgm in
  if !print_desugar then (
    print_endline "== Converted Program ==";
    Xexp.print desugared_pgm;
    print_newline ()
  );
  print_endline "== Running input program with xexp Interpreter ==";
  let orig_result = Xexp.run input_pgm in
  (match orig_result with
  | Xexp.Exn n -> print_endline ("Unhandled Exception #" ^ string_of_int n)
  | Xexp.Val (Xexp.N n) -> print_endline (string_of_int n)
  | Xexp.Val _ -> print_endline "Program is not evaluated to a number");
  let sugarless = Xexp.is_sugarless desugared_pgm in
  if Xexp.is_sugarless desugared_pgm then
    print_endline "[Valid] Exception sugar removed successfully"
  else
    print_endline "[Invalid] Exception sugar is NOT removed";
  print_endline "== Running converted program with xexp Interpreter ==";
  let converted_result = Xexp.run desugared_pgm in
  (match Xexp.run desugared_pgm with
  | Xexp.Exn n -> print_endline ("Unhandled Exception #" ^ string_of_int n)
  | Xexp.Val (Xexp.N n) -> print_endline (string_of_int n)
  | Xexp.Val _ -> print_endline "Program is not evaluated to a number");
  let equal_result =
    match orig_result, converted_result with
    | (Xexp.Exn n, Xexp.Val (Xexp.N 201812)) -> true
    | (Xexp.Val (Xexp.N n), Xexp.Val (Xexp.N m)) when n == m -> true
    | (_, _) -> false
  in
  exit (if sugarless && equal_result then 0 else 1)

let _ = main ()
