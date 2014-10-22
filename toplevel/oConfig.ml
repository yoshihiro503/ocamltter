let filter tw =
  let ignores = [] in
  match tw#user#details with
  | None -> true
  | Some d ->
      not (List.mem d#screen_name ignores)

let watching_words =
  ["#Coq"; "OCamltter"; "ProofCafe"; "#OCaml"]

let coffee_break = ref 61.0 (* seconds *)
(* 2014/10 : home_timeline rate limit is 15/15mins 
   (search is 180/15mins)
*)

let talk = ref true

