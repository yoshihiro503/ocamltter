(** Change these values from toplevel or your ocamlinit.ml *)

let filter = ref (fun (tw : OCamltter_twitter.Api_intf.Tweet.t) ->
  let ignores = [] in
  match tw#user#details with
  | None -> true
  | Some d ->
      not (List.mem d#screen_name ignores))

let watching_words = ref ["#Coq"; "OCamltter"; "ProofCafe"; "#OCaml"]

let coffee_break = ref 61.0 (* seconds *)
(* 2014/10 : home_timeline rate limit is 15/15mins 
   (search is 180/15mins)
*)

let talk = ref false (* Turn on if you want to hear Twitter time line in Japanese *)
  
