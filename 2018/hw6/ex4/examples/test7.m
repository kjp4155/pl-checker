(* rec fun type error *)

let val g = fn x => (x = true) in
let rec f = fn x => (
  if (x = 1) then (g x) else (f (x - 1))
  ) in
  f 4
end
end
(* result : type error *)
