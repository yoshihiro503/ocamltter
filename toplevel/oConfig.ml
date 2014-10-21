let filter tw =
  let ignores = [] in
  match tw#user#details with
  | None -> true
  | Some d ->
      not (List.mem d#screen_name ignores)

let watching_words =
(*
  ["#Coq"; "OCamltter"; "ProofCafe"; "#OCaml"]
*)
  ["#OCaml"]

let coffee_break = ref 60.0 (* second *)
(* 2014/10 : coffee_break 30.0 secs are too short and exceed the limit *)

let talk = ref false
