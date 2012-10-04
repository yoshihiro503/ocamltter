open Util
open TwitterApi


let filter tw =
  let ignores = [] in
  not (List.mem (sname tw) ignores)

let watching_words =
  ["#Coq"; "OCamltter"; "ProofCafe"; "#OCaml"]
    
let coffee_break = ref 30.0 (* second *)

let table = [
  ("camloeba", "俺は天才キャミバ様");
  ("ocamltter", "オーキャメルッター");
  ("ocaml", "オーキャメル");
    ]

let talk = ref false
