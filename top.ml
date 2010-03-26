include Util
open Util.Date
open TwitterApi

module Http = Http
module Json = Json
module TwitterApi = TwitterApi

(*===================================*)
(*===================================*)

(* let username = ref "YOUR TWITTER ID" *)
let username = ref ""
let password = ref ""

let coffee_break = 40.0 (* second *)

(*===================================*)
(*===================================*)

let print_tl =
  let latest = ref None in
  fun () ->
    let is_new_post p =
      match !latest with
      | Some l -> Date.lt l p.date
      | None -> true
    in
    let tl =
      TwitterApi.home_timeline (!username, !password)
	+> List.filter is_new_post
    in
    begin match tl with
    | [] -> ()
    | newest::_ ->
	latest := Some newest.date
    end;
    List.rev tl +> List.map show
      +> List.iter print_endline

let l ?(c=20) () =
  TwitterApi.home_timeline ~count:c (!username, !password)
    +> List.rev +> List.map show
    +> List.iter print_endline

let u text = TwitterApi.update (!username, !password) text

let init () =
  if !username = "" then
    (prerr_string "your id: @"; flush stderr;
     username := read_line ());
  if !password = "" then
    (prerr_string "password: "; flush stderr;
     ignore @@ Sys.command "stty -echo";
     password := read_line ();
     ignore @@ Sys.command "stty echo");
  let rec loop () =
    begin try print_tl () with
    | e -> print_endline (Printexc.to_string e)
    end;
    Thread.delay coffee_break;
    loop ()
  in
  let t = Thread.create loop in
  t ()

let _ =
  init ()

let help () =
print_string
"commands:
  l()\tlist timeline
  l ~c:COUNT ()\tlist timeline
  u \"TEXT\"\tpost a new message
  help()\tprint this help
"
