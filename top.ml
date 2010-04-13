include Util
include Ocamltter

module Http = Http
module Json = Json
module TwitterApi = TwitterApi

let print p s = Format.print_string ("\"" ^ s ^ "\"");;

let main =

  (* username := "YOUR-TWITTER-ID" *)
  username := "";
  password := "";
  coffee_break := 40.0; (* second *)

  init ()
