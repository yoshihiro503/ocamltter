open Util
open Util.Date
open TwitterApi
open Http
module Tw = TwitterApi

let oauth_acc : (string * string * string) option ref = ref None
let conffile = ".ocamltter"

let authorize () = 
  let url, req_tok, req_sec = Tw.fetch_request_token () in
  print_endline "Please grant access to ocamltter ant get a PIN at :";
  print_endline ("  " ^ url);
  print_string "Give me a PIN: "; flush stdout;
  let verif = read_line () in
  let username, acc_tok, acc_sec =
    Tw.fetch_access_token req_tok req_sec verif
  in
  oauth_acc := Some (acc_tok, acc_sec, verif);
  print_endline ("Grant Success! Hello, @"^username^" !");
  open_out_with conffile (fun ch ->
    output_string ch (acc_tok^"\n"^acc_sec^"\n"^verif^"\n"));
  (acc_tok, acc_sec, verif)

module Cache = struct
  type t = (int64, tweet) Hashtbl.t
  let init () : t = Hashtbl.create 100
  let is_new (cache: t) (tw: tweet) =
    not (Hashtbl.mem cache (status_id tw))
  let add cache (tw: tweet) = Hashtbl.add cache (status_id tw) tw
end

let load () = open_in_with conffile (fun ch ->
  let tok = input_line ch in let sec=input_line ch in let verif=input_line ch in
  let acc=(tok,sec,verif) in
  oauth_acc := Some acc; acc)

let oauth () =
  match !oauth_acc with
  | None -> (try load () with _ -> authorize ())
  | Some a -> a

let setup () = ignore(oauth())

let tw_sort = List.sort Tw.tw_compare

let get_timeline ?(c=20) () =
try
    let tl1 =
      List.filter Config.filter @@ Tw.home_timeline ~count:c (oauth())
    in
    let tl2 =
      list_concatmap Tw.search Config.watching_words
    in
    tw_sort (tl1 @ tl2)
with
| e ->
    prerr_endline (!%"Ocamltter.print_tl [%s]" (Printexc.to_string e));
    raise e

let print_timeline tw =
  List.iter print_endline @@ List.map show_tweet tw

let reload () =
  print_endline "loading..."; flush stdout;
  get_timeline ()

let l ?(c=20) ?u () : Tw.tweet list =
  print_endline "loading..."; flush stdout;
  match u with
  | None -> tw_sort @@ Tw.home_timeline ~count:c (oauth())
  | Some user -> tw_sort @@ Tw.user_timeline (oauth()) user

let lc count = l ~c:count ()
let lu user  = l ~u:user  ()

let m ?(c=20) () : Tw.tweet list =
  print_endline "loading..."; flush stdout;
  tw_sort @@ Tw.mentions (oauth()) c
    
let u text =
  ignore @@ Tw.update (oauth()) text

let rt status_id =
  ignore @@ Tw.retweet (oauth()) (Int64.to_string status_id)

let re status_id text =
  ignore @@ Tw.update ~in_reply_to_status_id:(Int64.to_string status_id)
    (oauth()) text

let qt st_id comment =
  let tw = get_tweet st_id in
  re (status_id tw) (!%"%s QT @%s: %s" comment (sname tw) (text tw))

let s word = List.sort tw_compare @@ Tw.search word

let limit () = Tw.rate_limit_status ()

let help =
"commands:
  l()                list timeline
  lc N               list timeline(N lines)
  lu \"NAME\"          list NAME's timeline
  m()                list mentions (tweet containing @YOU)
  u \"TEXT\"           post a new message
  re ID \"TEXT\"       reply to ID
  rt ID              retweet ID
  qt ID \"TEXT\"       qt ID
  s \"WORD\"           search tweets by a WORD
  let CMD = ...      define a your own command CMD
  setup()            (re)authorize ocamltter
  help               print this help
"

let start_polling () =
  let cache = Cache.init () in
  let rec loop () =
    begin try
      let tl = List.filter (Cache.is_new cache) (get_timeline ~c:200 ()) in
      List.iter (Cache.add cache) tl;
      print_timeline tl
    with e -> print_endline (Printexc.to_string e)
    end;
    Thread.delay Config.coffee_break;
    loop ()
  in
  let t = Thread.create loop in
  t ()

(* see .ocamlinit *)
