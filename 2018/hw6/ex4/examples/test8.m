(* function type unification *)

let val f1 = fn x => x in
let val f2 = fn y => f1 true in
let val v = malloc (f1 false, f2 (malloc 2)) in
  v
end
end
end

(* result : Loc(bool,bool) *)
