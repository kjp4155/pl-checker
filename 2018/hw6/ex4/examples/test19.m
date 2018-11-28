(* for assign *)

let val v = malloc (malloc (1, 1)) in
  v := malloc (3, 5)
end

(* result : loc (int) *)
