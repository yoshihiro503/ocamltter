open Util
open Util.Date
open TwitterApi
open Http

let username = ref Config.username
let password = ref Config.password
let acc () = !username, !password

module Cash = struct
  type t = (string, timeline) Hashtbl.t
  let init () : t = Hashtbl.create 100
  let is_new (cash : t) (x : timeline) =
    not (Hashtbl.mem cash x.id)
  let add cash (p: timeline) = Hashtbl.add cash p.id p
end

let get_timeline ?(c=20) () =
try
    let tl1 =
      List.filter Config.filter @@ TwitterApi.home_timeline ~count:c (acc())
    in
    let tl2 =
      list_concatmap (TwitterApi.search (acc())) Config.watching_words
    in
    let post_comp p1 p2 = compare p1.date p2.date in
    List.sort post_comp @@ (tl1@tl2)
with
| e ->
    prerr_endline (!%"Ocamltter.print_tl [%s]" (Printexc.to_string e));
    raise e

let print_timeline tl =
  List.iter print_endline @@ List.map show_tl tl

let lc count = get_timeline ~c:count ()
let lu user = TwitterApi.user_timeline (acc()) user

let l ?(c=20) ?u () : TwitterApi.timeline list =
  match u with
  | None -> lc c
  | Some user -> lu user

let u text =
  ignore @@ TwitterApi.update (acc()) text

let rt status_id =
  ignore @@ TwitterApi.retweet (acc()) (Int64.to_string status_id)

let re status_id text =
  ignore @@ TwitterApi.update ~in_reply_to_status_id:(Int64.to_string status_id)
    (acc()) text

let qt status_id text =
  let tl = get_aline (acc()) status_id in
  u (!%"%s QT @%s: %s" text tl.sname tl.text)

let s word = TwitterApi.search (acc()) word
  
let help =
"commands:
  l()                list timeline
  lc N               list timeline(N lines)
  lu \"NAME\"          list NAME's timeline
  u \"TEXT\"           post a new message
  re ID \"TEXT\"       reply to ID
  rt ID              retweet ID
  qt ID \"TEXT\"       qt ID
  s \"WORD\"           search by a WORD
  let CMD = ...      define a your own command CMD
  login \"NAME\"       login
  help               print this help
"

let login ?(pass="") uname =
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
  end

let start_polling () =
  let cash = Cash.init () in
  let rec loop () =
    begin try
      let tl = List.filter (Cash.is_new cash) (get_timeline ~c:200 ()) in
      List.iter (Cash.add cash) tl;
      print_timeline tl
    with e -> print_endline (Printexc.to_string e)
    end;
    Thread.delay Config.coffee_break;
    loop ()
  in
  let t = Thread.create loop in
  t ()

let main =
  login ~pass:!password !username;
  prind_endline "connecting..."; flush stdout;
  start_polling()
