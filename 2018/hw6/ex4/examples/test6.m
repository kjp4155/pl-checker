(* malloc *)

let val l = malloc (fn x=> (1 + x)) in
  (!l) 2
end


(* result : int *)
