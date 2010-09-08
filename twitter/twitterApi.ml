open Util
open Util.Date
open Http
open Json
open Oauth
open OauthForTwitter

exception TwErr of string

type status_id = int64

type tweet =
  | U of (Date.t * string * status_id * string * string)

let date = function
  | U (d, _,_,_,_) -> d
let sname = function
  | U (_, sname,_,_,_) -> sname
let text = function
  | U (_, _, _, _, text) -> text
let status_id = function
  | U (_, _, id, _, _) -> id

let show_tweet t =
  let fmt d = !%"%02d/%02d %02d:%02d" (Date.mon d) (day d) (hour d) (min d) in
  !%" [%s] %s: %s %LdL" (fmt (date t)) (sname t) (text t) (status_id t)

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
    let date =
      Json.getf "created_at" j |> Json.as_string |> parse_date
    in
    let text =
      Json.getf "text" j |> Json.as_string
    in
    let id =
      Json.getf "id" j |> Json.as_float |> Int64.of_float
    in
    let sname =
      Json.getf "user" j |> Json.getf "screen_name" |> Json.as_string
    in
    let client =
      Json.getf "source" j |> Json.as_string
    in
    U (date, sname, id, client, text)

let json2timeline j =
  Json.as_list j |> List.map json2status

let catch_twerr (f: 'a -> Json.t) (x : 'a) =
  try
    let j = f x in
    match Json.getf_opt "error" j with
    | Some err ->
	let msg = Json.as_string err in
	raise (TwErr msg)
    | None -> j
  with
  | TwErr m as e -> raise e
  | e -> failwith ("unknown error: "^Printexc.to_string e)

let twitter (tok,sec,verif) ?(host="api.twitter.com") meth cmd params =
  let oauth = OauthForTwitter.oauth(tok,sec,verif) in
  let f () =
    Json.parse (Oauth.access oauth meth host cmd params)
  in
  catch_twerr f ()

let twitter_without_auth ?(host="api.twitter.com") meth cmd params =
  let f () =
    Http.conn host meth cmd params (fun _ ch -> slist "" id (read_all ch))
      |> Json.parse
  in
  catch_twerr f ()

let home_timeline ?since_id ?count oauth =
  let params = [("since_id",since_id); ("count", option_map sint count)]
      |> list_filter_map (function
	| (key, Some v) -> Some (key, v)
	| (_, None) -> None)
  in
  twitter oauth GET "/statuses/home_timeline.json" params
    |> json2timeline

let user_timeline ?since_id ?count oauth sname =
  let params = [("since_id",since_id); ("count", option_map sint count);
		("screen_name", Some sname)]
      |> list_filter_map (function
	| (key, Some v) -> Some (key, v)
	| (_, None) -> None)
  in
  twitter oauth GET "/statuses/user_timeline.json" params
    |> json2timeline

let show status_id =
  twitter_without_auth GET (!%"/statuses/show/%Ld.json" status_id) []
    |> json2status

let get_tweet = show

let mentions oauth count =
  let params = [("count", !%"%d" count)] in
  twitter oauth GET "/1/statuses/mentions.json" params
    |> json2timeline

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
  twitter oauth POST "/statuses/update.json" params

let retweet oauth status_id =
  twitter oauth POST (!%"/statuses/retweet/%s.json" status_id) []

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
      U (d, sname, id, "", text))

let rate_limit_status () =
  twitter_without_auth GET "/1/account/rate_limit_status.json" []

let fetch_request_token = OauthForTwitter.fetch_request_token
let fetch_access_token  = OauthForTwitter.fetch_access_token
