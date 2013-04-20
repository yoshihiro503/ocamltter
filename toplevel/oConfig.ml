let filter tw =
  let ignores = [] in
  match tw#user#details with
  | None -> true
  | Some d ->
      not (List.mem d#screen_name ignores)

let watching_words =
  ["#Coq"; "OCamltter"; "ProofCafe"; "#OCaml"]

let coffee_break = ref 30.0 (* second *)

let talk = ref true
