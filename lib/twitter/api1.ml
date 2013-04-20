open Spotlib.Spot
open Api_intf

open Util
open Util.Date
open Http
open Json
open Json_conv
open Meta_conv.Open
open Xml

(* Warning: API version 1 will not work after 2013/Mar/05 *)

(** {6 HTTP parameters} *)

let (~?) l = list_filter_map (function
  | (key, Some v) -> Some (key, v)
  | (_, None) -> None) l


(** {6 Twitter bases} *)

exception TwErr of string

type tweet =
  | U of tweet_base
  | RT of tweet_base * tweet_base
  | RE of tweet_base * status_id

and tweet_base = {
  date   : Date.t; 
  sname  : string; 
  id     : status_id; 
  client : Xml.xml; 
  text   : string;
  json   : Json.t
}
     

let sclient = function
  | Xml.PCData "web" -> "web"
  | Xml.Tag ("a", _, [PCData clname]) -> clname
  | otherwise -> Xml.show otherwise

let date = function
  | U u -> u.date
  | RT (rt, _) -> rt.date
  | RE (re, _) -> re.date

let sname = function
  | U u -> u.sname
  | RT (rt,_) -> rt.sname
  | RE (re,_) -> re.sname

let status_id = function
  | U u -> u.id
  | RT (rt,_) -> rt.id
  | RE (re,_) -> re.id

let client = function
  | U u -> u.client
  | RT (rt,_) -> rt.client
  | RE (re,_) -> re.client

let text = function
  | U u -> u.text
  | RT (rt,_) -> rt.text
  | RE (re,_) -> re.text

let json = function
  | U u -> u.json
  | RT (rt,_) -> rt.json
  | RE (re,_) -> re.json


let show_tweet =
  let fmt d = !%"%02d/%02d %02d:%02d:%02d" (Date.mon d) (day d) (hour d) (min d) (sec d) in
  function
    | U u -> !%" [%s] %s: %s %LdL %s" (fmt u.date) u.sname u.text u.id
	  (sclient u.client)
    | RT (rt, orig) ->
	!%" [%s] [RT]%s: %s %LdL [RT %s %LdL] %s" (fmt rt.date)
	  orig.sname orig.text orig.id rt.sname rt.id (sclient rt.client)
    | RE (re, reply_id) ->
	!%" [%s] %s: %s %LdL to %LdL %s" (fmt re.date) re.sname
	  re.text re.id reply_id (sclient re.client)

let tw_compare t1 t2 = compare (date t1) (date t2)
    
(* Api_intf.Tweet.t_of_json will replace this *)
let json2tweet j =
    let date j =
      Json.getf "created_at" j |> Json.as_string |> Time.from_string |> Time.to_unix
    in
    let text j =
      Json.getf "text" j |> Json.as_string |> Http.html_decode
    in
    let id j =
      Json.getf "id_str" j |> Json.as_string |> Int64.of_string
    in
    let sname j =
      Json.getf "user" j |> Json.getf "screen_name" |> Json.as_string
    in
    let client j =
      Json.getf "source" j |> Json.as_string |> Option.maybe Xml.parse_string
        |> Option.get_or_else (Xml.PCData "ParseERROR")
    in
(*    let reply j = Json.getf "in_reply_to_screen_name" j |> Json.as_string in*)
    let base j = {
      date=date j; sname=sname j; id=id j; client=client j;
      text=text j; json=j
    } in
    match getf_opt "retweeted_status" j, getf_opt "in_reply_to_status_id_str" j with
    | Some rt, _ -> RT (base j, base rt)
    | _, Some (String f) -> RE (base j, Int64.of_string f)
    | _ -> U (base j)


(* Api_intf.Tweet.ts_of_json will replace this *)
let json2timeline j =
  Json.as_list j |> List.map json2tweet

let catch_twerr (f: 'a -> Json.t) (x : 'a) =
    let j = f x in
    match Json.getf_opt "error" j with
    | Some err ->
	let msg = Json.as_string err in
	raise (TwErr msg)
    | None -> j

let parse_json ch =
  Json.parse_ch ch

let twitter_without_auth ?(host="api.twitter.com") meth cmd params =
  let f () =
    Http.conn host meth cmd params (fun _ ch -> parse_json ch)
  in
  catch_twerr f ()

let twitter_low ?(host="api.twitter.com") meth cmd params =
  Http.conn host meth cmd params (fun _ ch -> String.concat "\n" & read_all ch)

let twitter oauth ?(host="api.twitter.com") meth cmd params =
  prerr_endline cmd;
  List.iter (fun (k,v) -> Format.eprintf "%s=%s@." k v) params;
  let f () =
    Auth.access oauth meth host cmd params (fun _ ch -> parse_json ch)
  in
  catch_twerr f ()

(** {6 APIs} *)

(** {7 Access with Cursor} *)

(* This is now deprecated. Use Api11.Cursor interface *)

module Cursor : sig

  val gets 
    : ((string * string) list -> Json.t)  (** basic api func with params *)
    -> (Json.t -> 'a)                     (** the decoder of the content *)
    -> (string * string) list             (** params without cursor *)
    -> 'a Stream.t                        (** result as a lazy list *)

end = struct

  (* We keep using record, since it is not visible to the users *)
  type resp = { 
    next_cursor : int64;
    previous_cursor : int64;
    next_cursor_str : string;
    previous_cursor_str : string;
    rest : Json.t mc_leftovers;
  } with conv(json)

  type t = {
    next: (int64 * string) option;
    prev: (int64 * string) option;
    contents: Json.t
  }

  let get f ?cursor params = 
    let res = resp_of_json_exn & f & match cursor with
      | None -> params
      | Some (_, c) -> ("cursor", c) :: params
    in
    let get_cursor s = function
      | 0L -> None
      | i -> Some (i, s)
    in
    { next = get_cursor res.next_cursor_str res.next_cursor;
      prev = get_cursor res.previous_cursor_str res.previous_cursor;
      contents = Json.Object res.rest
    }	

  let gets f decode params =
    let rec k = function
      | None -> Stream.null
      | Some cursor_opt ->
          let r = get f ?cursor:cursor_opt params in
          let next = match r.next with
            | None -> None
            | (Some _ as n) -> Some n
          in
          Stream.cons (decode r.contents) (k next)
    in
    k (Some None)
end
 
(** {7 Timeline Methods} *)

let home_timeline ?since_id ?count ?page oauth =
  let params = ~? [ "since_id",since_id
                  ; "count", Option.map sint count
                  ; "page",Option.map sint page
                  ]
  in
  twitter oauth GET "/1/statuses/home_timeline.json" params
  |> json2timeline

let home_timeline' ?since_id ?count ?page oauth =
  let params = ~? [ "since_id" , since_id 
                  ; "count"    , Option.map sint count
                  ; "page"     , Option.map sint page
                  ]
  in
  twitter oauth GET "/1/statuses/home_timeline.json" params
  |> Tweet.ts_of_json

let print_home_timeline o =
  home_timeline' o |> Json_conv.from_Ok |> Api_intf.Tweet.format_ts Format.std_formatter;;

let user_timeline ?since_id ?count ?page ?max_id oauth sname =
  let params = ~? [ "since_id"    , since_id
                  ; "count"       , Option.map sint count
                  ; "screen_name" , Some sname
                  ; "page"        , Option.map sint page
                  ; "max_id"      , max_id 
                  ]
  in
  twitter oauth GET "/1/statuses/user_timeline.json" params
  |> json2timeline

let user_timeline' ?since_id ?count ?page ?max_id oauth sname =
  let params = ~? [ "since_id"    , since_id
                  ; "count"       , Option.map sint count
                  ; "screen_name" , Some sname
                  ; "page"        , Option.map sint page
                  ; "max_id"      , max_id 
                  ]
  in
  twitter oauth GET "/1/statuses/user_timeline.json" params
  |> Tweet.ts_of_json

let show status_id =
  twitter_without_auth GET (!%"/1/statuses/show/%Ld.json" status_id) []
  |> json2tweet

let show' status_id =
  twitter_without_auth GET (!%"/1/statuses/show/%Ld.json" status_id) []
  |> Tweet.ts_of_json

let show_low status_id =
  twitter_low GET (!%"/1/statuses/show/%Ld.json" status_id) []

let get_tweet = show

let get_tweet' = show'

let mentions oauth count =
  let params = [("count", !%"%d" count)] in
  twitter oauth GET "/1/statuses/mentions.json" params
  |> json2timeline

let mentions' oauth count =
  let params = [("count", !%"%d" count)] in
  twitter oauth GET "/1/statuses/mentions.json" params
  |> Tweet.ts_of_json

(** {7 Status Methods} *)

let update ?(in_reply_to_status_id) oauth text =
  let text = match in_reply_to_status_id with
    | Some id ->
        let t = get_tweet (Int64.of_string id) in
        !%"@%s %s" (sname t) text
    | None -> text
  in
  let params = ~? [("in_reply_to_status_id", in_reply_to_status_id);
		   ("status",Some text)]
  in
  twitter oauth POST "/1/statuses/update.json" params
  |> json2tweet

let update' ?(in_reply_to_status_id) oauth text =
  let text = match in_reply_to_status_id with
    | Some id ->
        let t = get_tweet (Int64.of_string id) in
        !%"@%s %s" (sname t) text
    | None -> text
  in
  let params = ~? [("in_reply_to_status_id", in_reply_to_status_id);
		   ("status",Some text)]
  in
  twitter oauth POST "/1/statuses/update.json" params
  |> Tweet.ts_of_json

let destroy oauth status_id =
  twitter oauth POST (!%"/1/statuses/destroy/%Ld.json" status_id) []

let retweet oauth status_id =
  twitter oauth POST (!%"/1/statuses/retweet/%s.json" status_id) []

(** {7 User Methods} *)

let users_lookup_names_raw oauth snames =
  twitter oauth GET "/1/users/lookup.json" 
    [("screen_name", String.concat "," snames)]

let users_lookup_names oauth snames =
  users_lookup_names_raw oauth snames
  |> User.ts_of_json_exn

let user_lookup_name oauth sname =
  users_lookup_names_raw oauth [sname]
  |> User.ts_of_json_exn
  |> function
      | [v] -> v
      | _ -> assert false

let users_lookup_uids_raw oauth uids =
  twitter oauth GET "/1/users/lookup.json" 
    [("user_id", String.concat "," (List.map Int64.to_string uids))]

let users_lookup_uids oauth uids =
  users_lookup_uids_raw oauth uids
  |> User.ts_of_json_exn

let user_lookup_uid oauth uid =
  users_lookup_uids_raw oauth [uid]
  |> User.ts_of_json_exn
  |> function
      | [v] -> v
      | _ -> assert false

let users_search oauth word page =
  twitter oauth GET "/1/users/search.json" [("q", word); ("page", !%"%d" page)]

(** {7 Friendship Methods} *)

let friendship_create oauth sname =
  twitter oauth POST "/1/friendships/create.json" [("screen_name",sname)]
let friendship_destroy oauth sname =
  twitter oauth POST "/1/friendships/destroy.json" [("screen_name",sname)]

let friendship_create_id oauth id =
  twitter oauth POST "/1/friendships/create.json" [("user_id",Printf.sprintf "%Ld" id)] 
  |> User.t_of_json


(** {7 Social Graph Methods} *)

module FriendsFollowers = struct

  type ids = { ids : int64 list } with conv(json)

  let f dir ?sname oauth =
    Cursor.gets (twitter oauth GET ("/1/" ^ dir ^ "/ids.json"))
      ids_of_json_exn
      (match sname with
      | Some sname -> [("screen_name", sname)]
      | None -> [])
    |> Stream.to_list
    |> List.concat_map (fun x -> x.ids) 

end

type ids = int64 list
let friends   = FriendsFollowers.f "friends"
let followers = FriendsFollowers.f "followers"

(** {7 Account Methods} *)

let rate_limit_status () =
  twitter_without_auth GET "/1/account/rate_limit_status.json" []

(** {7 Favorite Methods} *)

let favorites ?sname oauth =
  let params = match sname with
  | Some sname -> [("id", sname)] | _ -> []
  in
  twitter oauth GET "/1/favorites.json" params

let favorites_create oauth status_id =
  twitter oauth POST (!%"/1/favorites/create/%Ld.json" status_id) []

let favorites_destroy oauth status_id =
  twitter oauth POST (!%"/1/favorites/destroy/%Ld.json" status_id) []

(** {7 Spam Reporting Methods} *)

let report_spam oauth sname =
  twitter oauth POST "/1/report_spam.json" [("screen_name",sname)]

(** {7 Help Methods} *)

let help_test () =
  twitter_without_auth GET "/1/help/test.json" []

(** {7 Search API Methods} *)

(* 1.0 *)
let search ?(rpp=20) ?(since_id) word =
  let params = Option.cat_options [
    Some("q",word);
    Some("rpp", !%"%d" rpp);
    Option.map (fun sid -> ("since_id", !%"%Ld" sid)) since_id;
  ] in
  twitter_without_auth GET ~host:"search.twitter.com" "/search.json" params
    |> Json.getf "results"
    |> Json.as_list
    |> List.map (fun j ->
      let d = Json.getf "created_at" j |> Json.as_string |> Time.from_string |> Time.to_unix in
      let sname = Json.as_string & Json.getf "from_user" j in
      let text = Http.html_decode & Json.as_string & Json.getf "text" j in
      let id = Int64.of_string & Json.as_string & Json.getf"id_str" j in
      let client =
	Xml.parse_string & Http.html_decode & Json.as_string
	& Json.getf "source" j
      in
      U {date=d; sname=sname; text=text; client=client;
	 id=id; json=j}
		)
