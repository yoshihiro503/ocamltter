open Util
open Util.Date
open TwitterApi
open Http

let username = ref ""
let password = ref ""

let coffee_break = ref 30.0

let print_tl =
  let latest = ref None in
  fun ?(c=10) () ->
    let is_new_post p =
      match !latest with
      | Some l -> Date.lt l p.date
      | None -> true
    in
    let tl =
      TwitterApi.home_timeline ~count:c (!username, !password)
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

let u text =
  let text' =
    if String.length text > 140 then
      (prerr_endline "140 over!";
      String.sub text 0 137 ^ "...")
    else text
  in
  TwitterApi.update (!username, !password) text'

let rt status_id =
  TwitterApi.retweet (!username, !password) (Int64.to_string status_id)

let re status_id text =
  TwitterApi.update ~in_reply_to_status_id:(Int64.to_string status_id)
    (!username, !password) text

let qt status_id text =
  let tl = get_aline (!username, !password) status_id in
  u (!%"%s QT @%s: %s" text tl.sname tl.text)

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
    Thread.delay !coffee_break;
    begin try print_tl () with
    | e -> print_endline (Printexc.to_string e)
    end;
    loop ()
  in
  let t = Thread.create loop in
  print_newline();
  print_tl ~c:50 ();
  t ()

let help () =
print_string
"commands:
  l()    \tlist timeline
  l ~c:N ()\tlist timeline
  u \"TEXT\"\tpost a new message
  re ID \"TEXT\"\treply to ID
  rt ID \tretweet ID
  qt ID \"TEXT\"\tqt ID
  help()\tprint this help
"
