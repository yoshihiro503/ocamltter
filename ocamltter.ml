open Ocaml_conv
open Spotlib.Spot

open Twitter
open Util
module TTS = GoogleTTS

module Consumer = Auth.Consumer

module Auth : sig

  (** Full auth information *)    
  type t = { username : string; oauth : Oauth.t; } with conv(ocaml)

  val authorize : Consumer.t -> Auth.VerifiedToken.t -> t
  val authorize_interactive : string (** appname *) -> Consumer.t -> t

  val load : string -> t list
  val save : string -> t list -> unit

  (** Simple implementation for one app + one account *)
  module Single : sig
    val save : string -> t -> unit
    val load : string -> Consumer.t -> Oauth.t
    val oauth : path:string -> appname:string -> Consumer.t -> Oauth.t
  end

end = struct

  type t = { username : string; oauth : Oauth.t } with conv(ocaml)

  let authorize app (_, verif as verified_token) = 
    let username, token = Auth.fetch_access_token app verified_token in
    let oauth = Auth.oauth app (token, verif) in
    { username; oauth }
  
  let authorize_interactive appname app = 
    let url, req_resp_token = Auth.fetch_request_token app in
    print_endline & "Please grant access to " ^ appname ^ " and get a PIN at :";
    print_endline & "  " ^ url;
    print_string "Give me a PIN: "; flush stdout;
    let verif = read_line () in
    let t = authorize app (req_resp_token, verif) in
    print_endline ("Grant Success! Hello, @"^t.username^" !");
    t
  
  module Single = struct
    (** It forgets username and consumer *)
    let save path t = 
      open_out_with path (fun ch ->
        output_string ch & String.concat "\n" [ t.oauth.Oauth.access_token;
                                                t.oauth.Oauth.access_token_secret;
                                                t.oauth.Oauth.verif;
                                                "" ])
  
    let load path app = open_in_with path (fun ch ->
      let token    = input_line ch in 
      let secret   = input_line ch in 
      let verif    = input_line ch in
      { Oauth.consumer_key  = app.Auth.Consumer.key;
        consumer_secret     = app.Auth.Consumer.secret;
        access_token        = token;
        access_token_secret = secret;
        verif               = verif })

    let oauth ~path ~appname app = try load path app with _ -> 
      let t = authorize_interactive appname app in
      save path t;
      t.oauth
  end

  let load path = 
    with_final 
      (open_in path)
      (fun ic -> List.map t_of_ocaml_exn (Ocaml.Parser.from_channel ic))
      close_in
      
  let save path ts = 
    with_final
      (open_out_gen [Open_wronly] 0o700 path)
      (fun oc -> 
        let ppf = Format.formatter_of_out_channel oc in
        List.iter (fun t -> Ocaml.format_with ~no_poly:true ocaml_of_t ppf t) ts)
      close_out

end

let application_name = "ocamltter"
let config_file = ref "Assign a conf filename."

let cached_oauth = ref None

let get_oauth () = match !cached_oauth with
  | Some oauth -> oauth
  | None ->
      let oauth = Auth.Single.oauth ~path:!config_file ~appname:application_name Consumer.ocamltter in
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
                   | `Json of Json.t Meta_conv.Error.t ]

(* CR jfuruse: should be moved to Meta_conv *)      
(* CR jfuruse: should print the warning *)
let from_result def = function
  | `Ok v -> v
  | `Error _e -> def

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
        let  ts = from_result [] 
          & Search.tweets ~count:c ?since_id word o >>| fun x -> x#statuses in
        if verbose then
          (print_endline (!%"%d" (List.length ts)); flush stdout);
        ts
      in
      list_concatmap search OConfig.watching_words
    in
    let tl2 =
      if verbose then (print_endline "loading..."; flush stdout);
      List.filter (fun _ -> true) (* OConfig.filter  *) (* CR jfuruse: need fix *)
      & from_result [] 
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
  tw_sort & from_result [] & match u with
  | None -> Timelines.home_timeline ~count:c o
  | Some screen_name -> Timelines.user_timeline ~count:c ~screen_name o

let lc (* ?page *) count = l ~c:count (* ?page *) ()
let lu (* ?page *) user  = l ~u:user  (* ?page *) ()

let m ?(c=20) () : tweet list =
  let o = get_oauth () in
  print_endline "loading..."; flush stdout;
  tw_sort & from_result [] & Timelines.mentions_timeline ~count:c o

let kwsk id =
  let o = get_oauth () in
  let rec iter store id =
    match Tweets.show id o with
    | `Error _ -> store
    | `Ok tw -> 
        match tw#in_reply_to_status_id with
        | Some id -> iter (tw :: store) id
        | None -> tw :: store
  in
  iter [] id
    
let u text =
  let o = get_oauth () in
  Tweets.update text o |> from_Ok |> fun x -> x#id

let re status_id text =
  let o = get_oauth () in
  Tweets.update ~in_reply_to_status_id:status_id text o |> from_Ok |> fun x -> x#id

let rt status_id =
  let o = get_oauth () in
  Tweets.retweet status_id o |> from_Ok |> fun x -> x#id

let del id = 
  let o = get_oauth () in
  Tweets.destroy id o |> from_Ok |> ignore

let qt st_id comment =
  let o = get_oauth () in
  Tweets.show st_id o 
  |> from_Ok 
  |> fun tw -> u (!%"%s QT @%s: %s" comment (from_Some tw#user#details)#screen_name tw#text)

let link id =
  let o = get_oauth () in
  Tweets.show id o |> from_Ok
  |> fun tw -> !%"http://twitter.com/%s/status/%Ld" (from_Some tw#user#details)#screen_name id
  
let qtlink id s =
  link id 
  |> fun url -> u @@ s ^ " QT " ^ url;;

let reqt st_id comment =
  let o = get_oauth () in
  Tweets.show st_id o |> from_Ok |> fun tw -> 
    re st_id & !%"%s QT @%s: %s" comment (from_Some tw#user#details)#screen_name tw#text

let follow sname = Friendships.create ~screen_name:sname (get_oauth ()) |> from_Ok

let unfollow sname = Friendships.destroy ~screen_name:sname (get_oauth ()) |> from_Ok

let favs screen_name = Favorites.list ~screen_name (get_oauth ()) |> from_Ok

let fav id = Favorites.create id (get_oauth ()) |> from_Ok |> fun x -> x#id
let unfav id = Favorites.destroy id (get_oauth ()) |> from_Ok |> fun x -> x#id
let frt id = ignore & fav id; rt id

(*
let report_spam sname =
  ignore @@ Api1.report_spam (get_oauth ()) sname
*)

let s word = 
  let o = get_oauth () in
  Search.tweets ~count:100 word o 
  |> from_Ok
  |> fun x -> x#statuses

(*
let limit_status () = Api1.rate_limit_status ()
*)


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
              print_endline & 
                !%"%s, %s" (from_Some t#user#details)#screen_name t#text; (* CR jfuruse: better printing *)
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


