open Spotlib.Spot
module Spot = Spotlib.Spot
open OCamltter_oauth
open Util
open OCamltter_twitter
module TTS = GoogleTTS

module Oauth = struct
  include Oauth_ex.Make(struct
    include OCamltter_twitter.Conf
    let app = { Oauth_ex.Consumer.key = "vS0nKAS6ieWL76zZaQgF4A";
                secret = "XHa1ZiPcNRsYKw4mdIv8wHUiNulpBFxKT1ntXXuJgo"; }
  end)

  let load auth_file =
    match Ocaml.load_with (Util.to_result *< Access_token.t_of_ocaml) auth_file with
    | Ok [a] -> a
    | _ -> assert false
  
  let get_acc_token auth_file =
    try load auth_file with
    | _ -> 
        let _res, acc_token = authorize_cli_interactive () in
        Ocaml.save_with Access_token.ocaml_of_t ~perm:0o600 auth_file [acc_token];
        acc_token
  
  let get auth_file =
    let acc_token = get_acc_token auth_file in
    oauth Conf.app acc_token
end

let config_file = ref "ocamltter.toplevel.auth"

let cached_oauth = ref None

let get_oauth () = match !cached_oauth with
  | Some oauth -> oauth
  | None ->
      let oauth = Oauth.get !config_file in
      cached_oauth := Some oauth;
      oauth
      
let setup () = 
  prerr_endline "getting oauth...";
  cached_oauth := None;
  let o = get_oauth () in
  prerr_endline "oauth done";
  o

open Api_intf
open Api11

open Meta_conv.Result.Open

type tweet = Api_intf.Tweet.t

module Cache : sig
  type t (** Carries "known tweets" to the system *)

  val init : unit -> t
  val is_new : t -> tweet -> bool
  val add : t -> tweet -> unit
(* CR jfuruse: not used 
  val qfilter : (tweet -> bool) -> t -> t 
  val qfind   : (tweet -> bool) -> t -> tweet option
*)
end = struct
  let max_size = 10000
  type t = tweet Queue.t
  let init () : t = Queue.create ()
  let qfilter f q =
    let q2 = Queue.create () in
    Queue.iter (fun x -> if f x then Queue.push x q2) q;
    q2
  let _qfind f q = try Some (Queue.pop @@ qfilter f q) with _ -> None
  let is_new (cache: t) (tw: tweet) =
    Queue.is_empty (qfilter (fun t -> t#id = tw#id) cache)
  let add cache (tw: tweet) =
    if Queue.length cache > max_size then ignore @@ Queue.pop cache;
    Queue.push tw cache
end

exception Error of Api11.Error.t

let default def v = v |> Result.result id (fun e ->
  Format.eprintf "%a@." Api11.Error.format e;
  def)

let from_Ok = function
  | `Ok v -> v
  | `Error e -> raise (Error e)

let from_Some = function
  | Some v -> v
  | _ -> assert false

let tw_compare (t1 : Tweet.t) t2 = 
  Time.compare t1#created_at t2#created_at

let tw_sort = List.sort tw_compare

let get_timeline ?(c=20) ?since_id verbose =
  let o = get_oauth () in
  try
    let tl1 =
      let search word =
        if verbose then
          (print_string (!%"searching with '%s'... " word); flush stdout);
        let  ts = default []
          & Search.tweets o ~count:c ?since_id word >>| fun x -> x#statuses in
        if verbose then
          (print_endline (!%"%d" (List.length ts)); flush stdout);
        ts
      in
      list_concatmap search !OConfig.watching_words
    in
    let tl2 =
      if verbose then (print_endline "loading..."; flush stdout);
      List.filter !OConfig.filter
      & default []
      & Timelines.home_timeline ~count:c (get_oauth ())
    in
    tw_sort (tl1 @ tl2)
  with
  | e ->
      prerr_endline "Ocamltter.get_timeline ERR:";
      raise e

(* formatter for REPL *)

let format_tweet ppf t = 
  Format.fprintf ppf "{ @[<v>user=%S; id=%LdL;@ text=\"%s\"@] }"
    (from_Some t#user#details)#screen_name 
    t#id
    t#text

let print_timeline tws =
  List.iter (Format.printf "%a@." format_tweet) tws

let reload () = get_timeline true

let l ?(c=20) ?u (* ?page *) () : tweet list =
  print_endline "loading..."; flush stdout;
  let o = get_oauth () in
  tw_sort & default [] & match u with
  | None -> Timelines.home_timeline ~count:c o
  | Some screen_name -> Timelines.user_timeline ~count:c ~screen_name o

let lc (* ?page *) count = l ~c:count (* ?page *) ()
let lu (* ?page *) user  = l ~u:user  (* ?page *) ()

let m ?(c=20) () : tweet list =
  let o = get_oauth () in
  print_endline "loading..."; flush stdout;
  tw_sort & default [] & Timelines.mentions_timeline ~count:c o

let kwsk id =
  let o = get_oauth () in
  let rec iter store id =
    match Tweets.show o id with
    | `Error _ -> store
    | `Ok tw -> 
        match tw#in_reply_to_status_id with
        | Some id -> iter (tw :: store) id
        | None -> tw :: store
  in
  iter [] id
    
let u text =
  let o = get_oauth () in
  from_Ok & Tweets.update o text >>| fun x -> x#id

let show id =
  let o = get_oauth () in
  Tweets.show o id |> from_Ok

let get_screen_name id =
  show id
  |> fun t -> t#user#details
  |> from_Some
  |> fun u -> u#screen_name

let re status_id text =
  let o = get_oauth () in
  let name = get_screen_name status_id in
  Tweets.update o ~in_reply_to_status_id:status_id ("@" ^ name ^ " " ^ text)
  |> from_Ok |> fun x -> x#id

let rt status_id =
  let o = get_oauth () in
  from_Ok
  & Tweets.retweet o status_id >>| fun x -> x#id

let del id = 
  let o = get_oauth () in
  Tweets.destroy o id |> from_Ok |> ignore

let qt st_id comment =
  let o = get_oauth () in
  Tweets.show o st_id
  |> from_Ok |> fun tw -> 
    u (!%"%s QT @%s: %s" comment (from_Some tw#user#details)#screen_name tw#text)

let link id =
  let o = get_oauth () in
  Tweets.show o id 
  |> from_Ok |> fun tw -> 
    !%"http://twitter.com/%s/status/%Ld" (from_Some tw#user#details)#screen_name id
  
let qtlink id s =
  link id |> fun url -> u @@ s ^ " QT " ^ url;;

let reqt st_id comment =
  let o = get_oauth () in
  Tweets.show o st_id 
  |> from_Ok |> fun tw -> 
    re st_id & !%"%s QT @%s: %s" comment (from_Some tw#user#details)#screen_name tw#text

let follow sname = Friendships.create ~screen_name:sname (get_oauth ()) |> from_Ok
let unfollow sname = Friendships.destroy ~screen_name:sname (get_oauth ()) |> from_Ok

let favs screen_name = Favorites.list ~screen_name (get_oauth ()) |> from_Ok

let fav id = Favorites.create (get_oauth ()) id |> from_Ok |> fun x -> x#id
let unfav id = Favorites.destroy (get_oauth ()) id |> from_Ok |> fun x -> x#id
let frt id = ignore & fav id; rt id

let report_spam sname = 
  SpamReporting.report_spam (get_oauth ()) ~screen_name: sname |> from_Ok

let s word = 
  let o = get_oauth () in
  Search.tweets o ~count:100 word  
  |> from_Ok |> fun x -> x#statuses

let limit_status () = Help.rate_limit_status (get_oauth ()) |> from_Ok


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
  #quit                quit ocamltter
"

let is_polling_on = ref false

let stop_polling ()  = is_polling_on := false

let print_tweet (t:Tweet.t) =
  let time =
    let tm = Time.to_unix t#created_at in
    !%"[%02d/%02d %02d:%02d]" (Date.mon tm) (Date.day tm) (Date.hour tm) (Date.min tm)
  in
  let name = (from_Some t#user#details)#screen_name in
  print_endline &
    !%"%s %s : %s %LdL %s" time name t#text t#id (Client.name t#source)

let start_polling () =
  is_polling_on := true;
  let cache = Cache.init () in
  let rec loop ?last_id verbose c =
    if !is_polling_on = true then begin
      let get () =
        let tl =
          List.filter (Cache.is_new cache)
          & get_timeline ~c ?since_id:last_id verbose
        in
        List.iter begin fun t ->
          Cache.add cache t;
        end tl;
        tl
      in
      let last_id = 
        match get () with
        | [] -> print_string "."; flush stdout; last_id
        | tl ->
            List.iter (fun t ->
              print_tweet t;
              if !OConfig.talk then TTS.say_ja (!%"%s, %s" (from_Some t#user#details)#screen_name t#text)) tl;
            print_endline "";
            Some (list_last tl)#id
        (* Error should be printed by get *)                
        (* | Inr e ->  
               print_endline (Printexc.to_string e);
               last_id
        *)
      in
      Thread.delay !OConfig.coffee_break;
      loop false 20 ?last_id (* verbose is only true at first time *)
    end
  in
  let t = Thread.create (fun () -> loop true 20) in
  t ()

(* see .ocamlinit *)


