open Ocaml_conv
open Spotlib.Spot

open Twitter
open Util
open Api
module TTS = GoogleTTS

module Consumer = Auth.Consumer

module Auth : sig
  type t = { username : string; oauth : Oauth.t; } with conv(ocaml)

  val authorize : Consumer.t -> Auth.VerifiedToken.t -> t
  val authorize_interactive : string (** appname *) -> Consumer.t -> t

  val load : string -> t list
  val save : string -> t list -> unit

  module Single : sig
    val save : string -> t -> unit
    val load : string -> Consumer.t -> Oauth.t
    val oauth : path:string -> appname:string -> Consumer.t -> Oauth.t
  end

end = struct

  (** Full auth information *)    
  type t = { username : string; oauth : Oauth.t } with conv(ocaml)

  let authorize app (_, verif as verified_token) = 
    let username, token = Api.fetch_access_token app verified_token in
    let oauth = Auth.oauth app (token, verif) in
    { username; oauth }
  
  let authorize_interactive appname app = 
    let url, req_resp_token = Api.fetch_request_token app in
    print_endline & "Please grant access to " ^ appname ^ " and get a PIN at :";
    print_endline & "  " ^ url;
    print_string "Give me a PIN: "; flush stdout;
    let verif = read_line () in
    let t = authorize app (req_resp_token, verif) in
    print_endline ("Grant Success! Hello, @"^t.username^" !");
    t
  
  (** Simple implementation for one app + one account *)
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

module Cache = struct
  let max_size = 10000
  type t = tweet Queue.t
  let init () : t = Queue.create ()
  let qfilter f q =
    let q2 = Queue.create () in
    Queue.iter (fun x -> if f x then Queue.push x q2) q;
    q2
  let _qfind f q = try Some (Queue.pop @@ qfilter f q) with _ -> None
  let is_new (cache: t) (tw: tweet) =
    Queue.is_empty (qfilter (fun t -> status_id t = status_id tw) cache)
  let add cache (tw: tweet) =
    if Queue.length cache > max_size then ignore @@ Queue.pop cache;
    Queue.push tw cache
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

let tw_sort = List.sort Api.tw_compare

let get_timeline ?(c=20) ?since_id verbose =
  try
    let tl1 =
      let search word =
	if verbose then
          (print_string (!%"searching with '%s'... " word); flush stdout);
	let ts = value_or [] @@ maybe (Api.search ~rpp:c ?since_id) word in
	if verbose then
          (print_endline (!%"%d" (List.length ts)); flush stdout);
	ts
      in
      list_concatmap search OConfig.watching_words
    in
    let tl2 =
      if verbose then (print_endline "loading..."; flush stdout);
      List.filter OConfig.filter @@ Api.home_timeline ~count:c (get_oauth ())
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

let l ?(c=20) ?u ?page () : Api.tweet list =
  print_endline "loading..."; flush stdout;
  match u with
  | None -> tw_sort @@ Api.home_timeline ~count:c ?page (get_oauth ())
  | Some user -> tw_sort @@ Api.user_timeline ?page (get_oauth ()) user

let lc ?page count = l ~c:count ?page ()
let lu ?page user  = l ~u:user  ?page ()

let m ?(c=20) () : Api.tweet list =
  print_endline "loading..."; flush stdout;
  tw_sort @@ Api.mentions (get_oauth ()) c

let kwsk id =
  let rec iter store id =
    match show id with
    | RE(_, re_id) as t -> iter (t :: store) re_id
    | t -> t :: store
  in
  iter [] id
    
let u text =
  Api.update (get_oauth ()) text |> Api.status_id

let re status_id text =
  Api.update ~in_reply_to_status_id:(Int64.to_string status_id) (get_oauth ()) text
  |> Api.status_id
    

let rt status_id =
  Api.retweet (get_oauth ()) (Int64.to_string status_id) |> ignore

let del id =
  ignore @@ Api.destroy (get_oauth ()) id

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
  ignore @@ Api.friendship_create (get_oauth ()) sname

let unfollow sname =
  ignore @@ Api.friendship_destroy (get_oauth ()) sname

let fav id =
  ignore @@ Api.favorites_create (get_oauth ()) id

let frt id = fav id; rt id

let report_spam sname =
  ignore @@ Api.report_spam (get_oauth ()) sname

let s word = List.sort tw_compare @@ Api.search ~rpp:100 word

let limit_status () = Api.rate_limit_status ()

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


(* CR jfuruse: this code no longer works, and it is also an application.
type ids = int64 list

let save_friends () =
  let o = get_oauth () in
  let friends = Api.friends o in
  Sexplib.Sexp.save_hum "friends.sexp" (sexp_of_ids friends)
  
let load_friends () =
  Sexplib.Sexp.load_sexp_conv_exn "friends.sexp" ids_of_sexp

type _res = Api_intf.User.t

let sync_friends () =
  let o = get_oauth () in
  let goal = load_friends () in
  let cur = Api.friends o in
  let add = List.filter (fun i -> not (List.mem i cur)) goal in
  List.iter (fun i ->
    Format.eprintf "adding %Ld@." i;
    try
    match Api.friendship_create_id o i with
    | `Ok _res -> 
(*
        Format.eprintf "RES: %a@." Sexplib.Sexp.pp_hum (sexp_of_res res);
*)
        Unix.sleep 1
    | `Error e ->
        Json_conv.format_full_error Format.stderr e;
        raise Exit
    with          
    | Api.TwErr s ->
        Format.eprintf "TW ERROR: %s@." s;
        Unix.sleep 1
  ) add
*)

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
	    print_endline (Api.show_tweet t);
            if !OConfig.talk then TTS.say_ja (!%"%s, %s" (sname t) (text t));
          end tl;
          print_endline "";
          Some (list_last tl |> Api.status_id)
      | Inl _ -> print_string "."; flush stdout; last_id
      | Inr e -> print_endline (Printexc.to_string e);
          last_id
      in
      Thread.delay !OConfig.coffee_break;
      loop false 20 ?last_id (* verbose is only true at first time *)
    end
  in
  let t = Thread.create (fun () -> loop true 20) in
  t ()

(* see .ocamlinit *)
