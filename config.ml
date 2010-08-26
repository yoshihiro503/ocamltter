include Util
include TwitterApi


let filter post =
  let ignores = [] in
  not (List.mem post.sname ignores)

let watching_words =
  ["#Coq"; "OCamltter"; "ProofCafe"; "#OCaml"; "CoqUn"; "Coq庵"]
    
let username = ""
let password = ""
let coffee_break = 30.0 (* second *)

