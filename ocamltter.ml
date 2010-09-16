open Util
open Util.Date
open TwitterApi
open Http
module Tw = TwitterApi

let oauth_acc : (string * string * string) option ref = ref None
let conffile =
  match maybe Sys.getenv "HOME" with
  | `Val home -> home ^ "/.ocamltter"
  | `Err e -> ".ocamltter"

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
  let max_size = 1000
  type t = tweet Queue.t
  let init () : t = Queue.create ()
  let qfilter f q =
    let q2 = Queue.create () in
    Queue.iter (fun x -> if f x then Queue.push x q2) q;
    q2
  let qfind f q = try Some (Queue.pop @@ qfilter f q) with _ -> None
  let is_new (cache: t) (tw: tweet) =
    Queue.is_empty (qfilter (fun t -> status_id t = status_id tw) cache)
  let add cache (tw: tweet) =
    if Queue.length cache > max_size then ignore @@ Queue.pop cache;
    Queue.push tw cache
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

let get_timeline ?(c=20) verbose =
  try
    let tl1 =
      let search word =
	if verbose then
          (print_string (!%"searching with '%s'... " word); flush stdout);
	let ts = Tw.search word in
	if verbose then
          (print_endline (!%"%d" (List.length ts)); flush stdout);
	ts
      in
      list_concatmap search Config.watching_words
    in
    let tl2 =
      if verbose then (print_endline "loading..."; flush stdout);
      List.filter Config.filter @@ Tw.home_timeline ~count:c (oauth())
    in
    tw_sort (tl1 @ tl2)
  with
  | e ->
      prerr_endline "Ocamltter.get_timeline ERR:";
      raise e

let print_timeline tw =
  List.iter print_endline @@ List.map show_tweet tw

let reload () =
  get_timeline true

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

let re status_id text =
  ignore @@ Tw.update ~in_reply_to_status_id:(Int64.to_string status_id)
    (oauth()) text

let rt status_id =
  ignore @@ Tw.retweet (oauth()) (Int64.to_string status_id)

let del id =
  ignore @@ Tw.destroy (oauth()) id

let qt st_id comment =
  let tw = get_tweet st_id in
  u (!%"%s QT @%s: %s" comment (sname tw) (text tw))

let follow sname =
  ignore @@ Tw.friendship_create (oauth()) sname

let unfollow sname =
  ignore @@ Tw.friendship_destroy (oauth()) sname

let fav id =
  ignore @@ Tw.favorites_create (oauth()) id

let report_spam sname =
  ignore @@ Tw.report_spam (oauth()) sname

let s word = List.sort tw_compare @@ Tw.search word

let limit () = Tw.rate_limit_status ()

let help =
"commands:
  l()                  list timeline
  lc N                 list timeline(N lines)
  lu \"NAME\"            list NAME's timeline
  m()                  list mentions (tweet containing @YOU)
  u \"TEXT\"             post a new message
  re ID \"TEXT\"         reply to ID
  del ID               delete tweet of ID
  rt ID                retweet ID
  qt ID \"TEXT\"         qt ID
  follow \"NAME\"        follow NAME
  unfollow \"NAME\"      unfollow NAME
  fav ID               mark ID as favorites
  report_spam \"NAME\"   report NAME as a spam user
  s \"WORD\"             search tweets by a WORD
  let CMD = ...        define a your own command CMD
  setup()              (re)authorize ocamltter
  help                 print this help
  start_polling ()     start polling
  stop_polling ()      stop polling
  #quite               quite ocamltter
"

let is_polling_on = ref true

let stop_polling ()  = is_polling_on := false

let start_polling () =
  is_polling_on := true;
  let cache = Cache.init () in
  let rec loop verbose =
    if !is_polling_on = true then begin
      begin try
	let tl =
	  List.filter (Cache.is_new cache) (get_timeline ~c:100 verbose)
	in
	List.iter (Cache.add cache) tl;
	print_timeline tl
      with e -> print_endline (Printexc.to_string e);
      end;
      Thread.delay !Config.coffee_break;
      loop false (* verbose is only true at first time *)
    end
  in
  let t = Thread.create loop in
  t true

(* see .ocamlinit *)
