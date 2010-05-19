include Util
include TwitterApi


let filter post =
  let ignores = [] in
  not (List.mem post.sname ignores)

let watching_words =
  ["#Coq"; "OCamltter"; "ProofCafe"; "#OCaml"]
    
let username = ""
let password = ""
let coffee_break = 60.0 (* second *)
