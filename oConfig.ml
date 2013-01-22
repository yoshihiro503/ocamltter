open Twitter
open Api


let filter tw =
  let ignores = [] in
  not (List.mem (sname tw) ignores)

let watching_words =
(*
  ["#Coq"; "OCamltter"; "ProofCafe"; "#OCaml"]
*)
  []

let coffee_break = ref 30.0 (* second *)

let talk = ref true
