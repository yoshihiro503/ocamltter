open Util
open Util.Date
open Http
open Json
open Oauth
open OauthForTwitter

exception TwErr of string

type token = string * string * string

type status_id = int64

type tweet =
  | U of (Date.t * string * status_id * string * string * Json.t)
  | RT of
      (Date.t * string * status_id * string * string * string * Date.t *status_id * Json.t)
  | RE of
      (Date.t * string * status_id * string * string * string * status_id * Json.t)

let date = function
  | U (d, _,_,_,_,_) -> d
  | RT(_, _,_,_,_,_,d,_,_) -> d
  | RE(d, _,_,_,_,_,_,_) -> d

let sname = function
  | U (_, sname,_,_,_, _) -> sname
  | RT(_, _,_,_,_,sname,_,_,_) -> sname
  | RE(_, sname,_,_,_,_,_,_) -> sname

let status_id = function
  | U (_, _, id, _, _, _) -> id
  | RT(_, _,_,_,_,_,_,id,_) -> id
  | RE(_, _,id,_,_,_,_,_) -> id

let client = function
  | U (_, _, _, client,_, _) -> client
  | RT(_, _,_,client,_,_,_,_,_) -> client
  | RE(_, _,_,client,_,_,_,_) -> client
	
let text = function
  | U (_, _, _, _, text, _) -> text
  | RT(_, _,_,_,text,_,_,_,_) -> text
  | RE(_, _,_,_,text,_,_,_) -> text
	
let json = function
  | U (_, _, _, _, _, j) -> j
  | RT(_, _,_,_,_,_,_,_,j) -> j
  | RE(_, _,_,_,_,_,_,j) -> j


let show_tweet =
  let fmt d = !%"%02d/%02d %02d:%02d" (Date.mon d) (day d) (hour d) (min d) in
  function
    | U (d, n, sid, _, text, _) -> !%" [%s] %s: %s %LdL" (fmt d) n text sid
    | RT(d, n, sid, _, text, rter, rtd, rtid, _) ->
	!%" [%s] %s: %s %LdL [RT %s %LdL]"
(fmt d) n text sid rter rtid
    | RE(d, n, sid, _, text, re, reid, _) ->
	!%" [%s] %s: %s %LdL to %LdL" (fmt d) n text sid reid

let tw_compare t1 t2 = compare (date t1) (date t2)
    
let parse_date st =
  let parse_date01 st = (* Wed Apr 21 02:29:17 +0000 2010 *)
    let mon = pmonth @@ String.sub st 4 3 in
    let day = int_of_string @@ String.sub st 8 2 in
    let h = int_of_string @@ String.sub st 11 2 in
    let m = int_of_string @@ String.sub st 14 2 in
    let s = int_of_string @@ String.sub st 17 2 in
    let year = int_of_string @@ String.sub st 26 4 in
    Date.make_from_gmt year mon day h m s
  in
  let parse_date02 st = (* Sat, 17 Apr 2010 08:23:55 +0000 *)
    let mon = pmonth @@ String.sub st 8 3 in
    let day = int_of_string @@ String.sub st 5 2 in
    let h = int_of_string @@ String.sub st 17 2 in
    let m = int_of_string @@ String.sub st 20 2 in
    let s = int_of_string @@ String.sub st 23 2 in
    let year = int_of_string @@ String.sub st 12 4 in
    Date.make_from_gmt year mon day h m s
  in
  try parse_date01 st with
  | _ -> parse_date02 st
	  
let json2status j =
    let date j =
      Json.getf "created_at" j |> Json.as_string |> parse_date
    in
    let text j =
      Json.getf "text" j |> Json.as_string
    in
    let id j =
      Json.getf "id" j |> Json.as_float |> Int64.of_float
    in
    let sname j =
      Json.getf "user" j |> Json.getf "screen_name" |> Json.as_string
    in
    let client j = Json.getf "source" j |> Json.as_string in
    let reply j = Json.getf "in_reply_to_screen_name" j |> Json.as_string in
    match getf_opt "retweeted_status" j, getf_opt "in_reply_to_status_id" j with
    | Some rt, _ ->
	RT (date rt, sname rt, id rt, client rt, text rt, sname j, date j, id j
	      , j)
    | _, Some (Number f) ->
	RE (date j, sname j, id j, client j, text j, reply j,
	    Int64.of_float f, j)
    | _ ->
	U (date j, sname j, id j, client j, text j, j)

let json2timeline j =
  Json.as_list j |> List.map json2status

let catch_twerr (f: 'a -> Json.t) (x : 'a) =
    let j = f x in
    match Json.getf_opt "error" j with
    | Some err ->
	let msg = Json.as_string err in
	raise (TwErr msg)
    | None -> j

let parse_json ch =
  Json.parse (slist "" id @@ read_all ch)

let twitter (tok,sec,verif) ?(host="api.twitter.com") meth cmd params =
  let oauth = OauthForTwitter.oauth(tok,sec,verif) in
  let f () =
    Oauth.access oauth meth host cmd params (fun _ ch -> parse_json ch)
  in
  catch_twerr f ()

let twitter_without_auth ?(host="api.twitter.com") meth cmd params =
  let f () =
    Http.conn host meth cmd params (fun _ ch -> parse_json ch)
  in
  catch_twerr f ()

(** {6 APIs} *)

(** {7 Timeline Methods} *)

let home_timeline ?since_id ?count oauth =
  let params = [("since_id",since_id); ("count", option_map sint count)]
      |> list_filter_map (function
	| (key, Some v) -> Some (key, v)
	| (_, None) -> None)
  in
  twitter oauth GET "/1/statuses/home_timeline.json" params
    |> json2timeline

let user_timeline ?since_id ?count oauth sname =
  let params = [("since_id",since_id); ("count", option_map sint count);
		("screen_name", Some sname)]
      |> list_filter_map (function
	| (key, Some v) -> Some (key, v)
	| (_, None) -> None)
  in
  twitter oauth GET "/1/statuses/user_timeline.json" params
    |> json2timeline

let show status_id =
  twitter_without_auth GET (!%"/1/statuses/show/%Ld.json" status_id) []
    |> json2status

let get_tweet = show

let mentions oauth count =
  let params = [("count", !%"%d" count)] in
  twitter oauth GET "/1/statuses/mentions.json" params
    |> json2timeline

(** {7 Status Methods} *)

let update ?(in_reply_to_status_id) oauth text =
  let text = match in_reply_to_status_id with
  | Some id ->
      let t = get_tweet (Int64.of_string id) in
      !%"@%s %s" (sname t) text
  | None -> text
  in
  let params = [("in_reply_to_status_id", in_reply_to_status_id);
		("status",Some text)]
      |> list_filter_map (function
	| (key, Some v) -> Some (key, v)
	| (_, None) -> None)
  in
  twitter oauth POST "/1/statuses/update.json" params

let destroy oauth status_id =
  twitter oauth POST (!%"/1/statuses/destroy/%Ld.json" status_id) []

let retweet oauth status_id =
  twitter oauth POST (!%"/1/statuses/retweet/%s.json" status_id) []

(** {7 User Methods} *)

let users_lookup oauth sname =
  twitter oauth GET "/1/users/lookup.json" [("screen_name", sname)]

let users_lookup_uid oauth uid =
  twitter oauth GET "/1/users/lookup.json" [("user_id", uid)]

(** {7 Friendship Methods} *)

let friendship_create oauth sname =
  twitter oauth POST "/1/friendships/create.json" [("screen_name",sname)]
let friendship_destroy oauth sname =
  twitter oauth POST "/1/friendships/destroy.json" [("screen_name",sname)]

(** {7 Social Graph Methods} *)

let friends ?sname oauth =
  let params =
    match sname with
    | Some sname -> [("screen_name", sname)]
    | None -> []
  in
  twitter oauth GET "/1/friends/ids.json" params

let followers ?sname oauth =
  let params =
    match sname with
    | Some sname -> [("screen_name", sname)]
    | None -> []
  in
  twitter oauth GET "/1/followers/ids.json" params

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

(** {7 OAuth Methods} *)

let fetch_request_token = OauthForTwitter.fetch_request_token
let fetch_access_token  = OauthForTwitter.fetch_access_token

(** {7 Help Methods} *)

let help_test () =
  twitter_without_auth GET "/1/help/test.json" []

(** {7 Search API Methods} *)

let search word =
  let ps = [("q",word);("rpp","100")] in
  twitter_without_auth GET ~host:"search.twitter.com" "/search.json" ps
    |> Json.getf "results"
    |> Json.as_list
    |> List.map (fun j ->
      let d = parse_date @@ Json.as_string @@ Json.getf "created_at" j in
      let sname = Json.as_string @@ Json.getf "from_user" j in
      let text = Json.as_string @@ Json.getf "text" j in
      let id = Int64.of_float @@ Json.as_float @@ Json.getf"id" j in
      U (d, sname, id, "", text, j))
