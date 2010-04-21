include Util
include Ocamltter

let print p s = Format.print_string ("\"" ^ s ^ "\"");;

let ptl p (tl:TwitterApi.timeline) =
  Format.print_string (TwitterApi.show_tl tl);;

let main =

  (* username := "YOUR-TWITTER-ID" *)
  let username = "" in
  let password = "" in
  let coffee_break = 40.0 in (* second *)

  init (username, password);
  polling coffee_break
