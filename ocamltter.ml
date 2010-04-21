open Util
open Util.Date
open TwitterApi
open Http

let username = ref ""
let password = ref ""

let get_timeline =
  let latest = ref None in
  fun c ->
    let debug = ref "" in
try
    let is_new_post p =
      match !latest with
      | Some l -> Date.lt l p.date
      | None -> true
    in
    debug := !debug ^ "get-tl";
    let tl =
      TwitterApi.home_timeline ~count:c (!username, !password)
	+> List.filter is_new_post
    in
    debug := !debug ^ " update-date";
    begin match tl with
    | [] -> ()
    | newest::_ ->
	latest := Some newest.date
    end;
    debug := !debug ^ " print";
    tl
with
| e ->
    prerr_endline (!%"Ocamltter.print_tl [%s] (%s)" (Printexc.to_string e) !debug);
    raise e

let print_timeline ?(c=20) () =
  List.iter print_endline @@ List.map show_tl
    @@ List.rev (get_timeline c)

let l ?(c=20) () : TwitterApi.timeline list =
  TwitterApi.home_timeline ~count:c (!username, !password)
    +> List.rev

let u text =
  let text' =
    if String.length text > 140 then
      (prerr_endline "may 140 over!";
(*      String.sub text 0 137 ^ "...")*)
       text)
    else text
  in
  ignore @@ TwitterApi.update (!username, !password) text'

let rt status_id =
  ignore @@ TwitterApi.retweet (!username, !password) (Int64.to_string status_id)

let re status_id text =
  ignore @@ TwitterApi.update ~in_reply_to_status_id:(Int64.to_string status_id)
    (!username, !password) text

let qt status_id text =
  let tl = get_aline (!username, !password) status_id in
  u (!%"%s QT @%s: %s" text tl.sname tl.text)

let init (uname, pass) =
  begin if uname = "" then
    (prerr_string "your id: @"; flush stderr;
    username := read_line ())
  else
    username := uname;
  end;
  begin if pass = "" then
    (prerr_string "password: "; flush stderr;
    ignore @@ Sys.command "stty -echo";
    password := read_line ();
    ignore @@ Sys.command "stty echo";
    print_newline())
  else
    password := pass;
  end;
  print_timeline ~c:50 ()

let polling coffee_break =
  let rec loop () =
    Thread.delay coffee_break;
    begin try print_timeline () with
    | e -> print_endline (Printexc.to_string e)
    end;
    loop ()
  in
  let t = Thread.create loop in
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

let acc () = !username, !password
  
