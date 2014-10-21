open Twitter;;
open Util;;
open Http;;
open Api11;;
open Ocamltter;;

(* Surpress String escaping of OCaml toplevel printer to see UTF-8 string in console *)
let print_string_literal p s = Format.print_string ("\"" ^ s ^ "\"");;

#install_printer print_string_literal;;
#install_printer format_tweet;;
#install_printer Api_intf.Rate_limit_status.format;;
#install_printer Api_intf.User.format;;

let () = config_file :=
  match maybe Sys.getenv "HOME" with
  | Inl home -> home ^ "/.ocamltter.toplevel.auth"
  | Inr e -> ".ocamltter.toplevel.auth"

let () = Format.eprintf "Using %s@." !config_file

let o = setup();;
start_polling ();;

open Api11;;

