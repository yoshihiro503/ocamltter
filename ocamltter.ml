open Util
open Util.Date
open TwitterApi
open Http
module Tw = TwitterApi
module TTS = GoogleTTS

let oauth_acc : (string * string * string) option ref = ref None
let config_file = ref "Assign a conf filename."

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
  open_out_with !config_file (fun ch ->
    output_string ch (acc_tok^"\n"^acc_sec^"\n"^verif^"\n"));
  (acc_tok, acc_sec, verif)

module Cache = struct
  let max_size = 10000
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

let load () = open_in_with !config_file (fun ch ->
  let tok = input_line ch in let sec=input_line ch in let verif=input_line ch in
  let acc=(tok,sec,verif) in
  oauth_acc := Some acc; acc)

let oauth () =
  match !oauth_acc with
  | None -> (try load () with _ -> authorize ())
  | Some a -> a

let setup () = ignore(oauth())

let tw_sort = List.sort Tw.tw_compare

let get_timeline ?(c=20) ?since_id verbose =
  try
    let tl1 =
      let search word =
	if verbose then
          (print_string (!%"searching with '%s'... " word); flush stdout);
	let ts = value_or [] @@ maybe (Tw.search ~rpp:c ?since_id) word in
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

let l ?(c=20) ?u ?page () : Tw.tweet list =
  print_endline "loading..."; flush stdout;
  match u with
  | None -> tw_sort @@ Tw.home_timeline ~count:c ?page (oauth())
  | Some user -> tw_sort @@ Tw.user_timeline ?page (oauth()) user

let lc ?page count = l ~c:count ?page ()
let lu ?page user  = l ~u:user  ?page ()

let m ?(c=20) () : Tw.tweet list =
  print_endline "loading..."; flush stdout;
  tw_sort @@ Tw.mentions (oauth()) c

let kwsk id =
  let rec iter store id =
    match show id with
    | RE(_, re_id) as t -> iter (t :: store) re_id
    | t -> t :: store
  in
  iter [] id
    
let u text =
  Tw.update (oauth()) text |> Tw.status_id

let re status_id text =
  Tw.update ~in_reply_to_status_id:(Int64.to_string status_id) (oauth()) text
  |> Tw.status_id
    

let rt status_id =
  Tw.retweet (oauth()) (Int64.to_string status_id) |> ignore

let del id =
  ignore @@ Tw.destroy (oauth()) id

let qt st_id comment =
  let tw = get_tweet st_id in
  u (!%"%s QT @%s: %s" comment (sname tw) (text tw))

let link id =
  let t = show id in
  !%"http://twitter.com/%s/status/%Ld" (sname t) id
  
let qtlink id s =
  u @@ s ^ " QT " ^ link id;;

let reqt st_id comment =
  let tw = get_tweet st_id in
  re st_id (!%"%s QT @%s: %s" comment (sname tw) (text tw))

let follow sname =
  ignore @@ Tw.friendship_create (oauth()) sname

let unfollow sname =
  ignore @@ Tw.friendship_destroy (oauth()) sname

let fav id =
  ignore @@ Tw.favorites_create (oauth()) id

let frt id = fav id; rt id

let report_spam sname =
  ignore @@ Tw.report_spam (oauth()) sname

let s word = List.sort tw_compare @@ Tw.search ~rpp:100 word

let limit_status () = Tw.rate_limit_status ()

let help =
"commands:
  l()                  list timeline
  lc N                 list timeline(N lines)
  lu \"NAME\"            list NAME's timeline
  m()                  list mentions (tweet containing @YOU)
  u \"TEXT\"             post a new message
  re ID \"TEXT\"         reply to ID
  del ID               delete tweet of ID
  rt ID                retweet to ID
  qt ID \"TEXT\"         qt ID
  qtlink ID \"TEXT\"     qt with link for ID
  follow \"NAME\"        follow NAME
  unfollow \"NAME\"      unfollow NAME
  fav ID               mark ID as favorites
  frt ID               fav ID and rt ID
  report_spam \"NAME\"   report NAME as a spam user
  s \"WORD\"             search tweets by a WORD
  show ID              show the tweet of ID
  link ID              link for the tweet of ID
  kwsk ID              show the convesation about ID
  setup()              (re)authorize ocamltter
  start_polling ()     start polling
  stop_polling ()      stop polling
  limit_status ()      rate limit status of API
  let CMD = ...        define a your own command CMD
  help                 print this help
  #quite               quite ocamltter
"

let is_polling_on = ref true

let stop_polling ()  = is_polling_on := false

let start_polling () =
  is_polling_on := true;
  let cache = Cache.init () in
  let rec loop ?last_id verbose c =
    if !is_polling_on = true then begin
      let get () =
	let tl =
	  List.filter (Cache.is_new cache)
            (get_timeline ~c:c ?since_id:last_id verbose)
	in
	List.iter begin fun t ->
          Cache.add cache t;
        end tl;
        tl
      in
      let last_id = match maybe get () with
      | Inl tl when List.length tl > 0 ->
          List.iter begin fun t ->
	    print_endline (Tw.show_tweet t);
            if !Config.talk then TTS.say_ja (!%"%s, %s" (sname t) (text t));
          end tl;
          print_endline "";
          Some (list_last tl |> Tw.status_id)
      | Inl _ -> print_string "."; flush stdout; last_id
      | Inr e -> print_endline (Printexc.to_string e);
          last_id
      in
      Thread.delay !Config.coffee_break;
      loop false 20 ?last_id (* verbose is only true at first time *)
    end
  in
  let t = Thread.create (fun () -> loop true 20) in
  t ()

(* see .ocamlinit *)
