open Ocaml_conv
open Spotlib.Spot
module Spot = Spotlib.Spot
open Twitter
open Util
module TTS = GoogleTTS

module Consumer = Auth.Consumer

module Auth = struct

  module App = struct
    type t = { name : string; consumer : Consumer.t } with conv(ocaml)
  
    let dummy = { name = "dummy app";
                  consumer = Consumer.dummy }
  
    let ocamltter = 
      { name = "ocamltter";
        consumer = {
          Consumer.key = "vS0nKAS6ieWL76zZaQgF4A";
          secret = "XHa1ZiPcNRsYKw4mdIv8wHUiNulpBFxKT1ntXXuJgo";
        }
      }
  end
  
  module User = struct
     type t = { token        : string;
                token_secret : string;
                verif        : string } with conv(ocaml)
  
    let dummy = { token = "123456789-Base64DataHereBase64DataHereBase64DataHe";
                  token_secret = "Base64DataHereBase64DataHereBase64DataHer";
                  verif = "9876543 (digit given from twitter auth page)" }
  end

  open Twitter.Auth

  type ('a,'b) hashtbl = ('a,'b) Hashtbl.t

  type t = { app: App.t; users: (string, User.t) hashtbl } with conv(ocaml)
  type tbl = (App.t, (string, User.t) hashtbl) Hashtbl.t

  let dummy = { app = App.dummy;
                users = Hashtbl.of_list 1 [ "dummy user", User.dummy ] }

  let oauth app user = 
    { Oauth.consumer_key  = app.App.consumer.Consumer.key;
      consumer_secret     = app.App.consumer.Consumer.secret;
      access_token        = user.User.token;
      access_token_secret = user.User.token_secret;
    }

  let load path = 
    let tbl = Hashtbl.create 17 in
    Ocaml.load_with_exn t_of_ocaml path
    |> List.iter (fun t -> Hashtbl.add tbl t.app t.users)
    |> fun () -> tbl

  let save path tbl = 
    Hashtbl.to_list tbl
    |> List.map (fun (k,v) -> { app=k; users= v })
    |> Ocaml.save_with ocaml_of_t ~perm:0o600 path

  let save_dummy path =
    if Sys.file_exists path then failwithf "%s: already exists" path;
    save path & Hashtbl.of_list 1 [dummy.app, dummy.users]

  let find (tbl:tbl) app user_name =
    Hashtbl.find_all tbl app
    |> Hashtbl.concat
    |> fun users -> Hashtbl.find_all users user_name

  let authorize app (_, verif as verified_token : VerifiedToken.t) = 
    let app_consumer = app.App.consumer in
    match Auth.fetch_access_token app_consumer verified_token with
    | `Ok (username, token) ->
        let oauth = Auth.oauth app_consumer (token, verif) in
        username, { User.token = oauth.Oauth.access_token;
                    token_secret = oauth.Oauth.access_token_secret;
                    verif = verif }
    | `Error (`Http (st, err)) ->
        failwithf "oauth http failed(%d): %s" st err
  
  let authorize_interactive app = 
    match Auth.fetch_request_token app.App.consumer with
    | `Ok (url, req_resp_token) ->
        print_endline & "Please grant access to " ^ app.App.name ^ " and get a PIN at :";
        print_endline & "  " ^ url;
        print_string "Give me a PIN: "; flush stdout;
        let verif = read_line () in
        let username, t = authorize app (req_resp_token, verif) in
        print_endline ("Grant Success! Hello, @"^username^" !");
        username, t
    | `Error (`Http (st, err)) ->
        failwithf "oauth http failed(%d): %s" st err

  module Single = struct
    (** It forgets username and consumer *)
    let save path t = 
      open_out_with path (fun ch ->
        output_string ch & String.concat "\n" [ t.User.token;
                                                t.User.token_secret;
                                                t.User.verif;
                                                "" ])
  
    let load path = open_in_with path & fun ch ->
      let token    = input_line ch in 
      let secret   = input_line ch in 
      let verif    = input_line ch in
      { User.token   = token;
        token_secret = secret;
        verif        = verif }
  
    let oauth path app = 
      let user = try load path with _ -> 
        let _username, t = authorize_interactive app in
        save path t;
        t
      in
      oauth app user
  end

end

let config_file = ref "Assign a conf filename."

let cached_oauth = ref None

let get_oauth () = match !cached_oauth with
  | Some oauth -> oauth
  | None ->
      let oauth = Auth.Single.oauth !config_file Auth.App.ocamltter in
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

exception Error of [`Http of int * string
                   | `Json of Json.t Meta_conv.Error.t 
                   | `Json_parse of exn * string
                   ]

let default def v = v |> Spot.result id (function
  | `Http (code, mes) ->
      Format.eprintf "HTTP error %d: %s@." code mes;
      def
  | `Json e ->
      Format.eprintf "@[JSON error:@ %a@]@."
        (Meta_conv.Error.format Json_conv.format) e;
      def
  | `Json_parse (exn, s) ->
      Format.eprintf "@[JSON error: %s@ %s@]@."
        (Printexc.to_string exn)
        s;
      def
  )

let from_Ok x = x |> Spot.from_Ok (fun e -> Error e)

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
      list_concatmap search OConfig.watching_words
    in
    let tl2 =
      if verbose then (print_endline "loading..."; flush stdout);
      List.filter OConfig.filter
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
  #quite               quite ocamltter
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


