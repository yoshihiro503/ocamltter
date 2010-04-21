open Util
open Util.Date
open Http
open Json

exception TwErr of string

type timeline = {
    date: Date.t;
    sname: string;
    id: string;
    clientname: string;
    text: string;
  }



(*
Wed Apr 21 02:29:17 +0000 2010
012345678901234567890123456789
*)

let parse_date01 st =
  let mon = pmonth @@ String.sub st 4 3 in
  let day = int_of_string @@ String.sub st 8 2 in
  let h = int_of_string @@ String.sub st 11 2 in
  let m = int_of_string @@ String.sub st 14 2 in
  let s = int_of_string @@ String.sub st 17 2 in
  let year = int_of_string @@ String.sub st 26 4 in
  Date.make_from_gmt year mon day h m s
(*
Sat, 17 Apr 2010 08:23:55 +0000
0123456789012345678901234567890
*)
let parse_date02 st =
  let mon = pmonth @@ String.sub st 8 3 in
  let day = int_of_string @@ String.sub st 5 2 in
  let h = int_of_string @@ String.sub st 17 2 in
  let m = int_of_string @@ String.sub st 20 2 in
  let s = int_of_string @@ String.sub st 23 2 in
  let year = int_of_string @@ String.sub st 12 4 in
  Date.make_from_gmt year mon day h m s

let parse_date st =
  try parse_date01 st with
  | _ -> parse_date02 st

let show_tl tl =
  let h,m = hour tl.date, min tl.date in
  !%" [%02d:%02d] %s: %s %sL" h m tl.sname tl.text tl.id

let json2status j =
    let date =
      Json.getf "created_at" j +> Json.as_string +> parse_date
    in
    let text =
      Json.getf "text" j +> Json.as_string
    in
    let id =
      Json.getf "id" j +> Json.as_float +> Int64.of_float +> Int64.to_string
    in
    let sname =
      Json.getf "user" j +> Json.getf "screen_name" +> Json.as_string
    in
    let client =
      Json.getf "source" j +> Json.as_string
    in
    {date=date; sname=sname; text=text; id=id; clientname=client}

let json2timeline j =
  Json.as_list j +> List.map json2status

let catch_twerr (f: 'a -> Json.t) (x : 'a) =
  try
    let j = f x in
    match Json.getf_opt "error" j with
    | Some err ->
	let msg = Json.as_string err in
(*      let req = Json.as_string @@ Json.getf "request" j in*)
	raise (TwErr msg)
    | None -> j
  with
  | TwErr m as e -> raise e
  | e -> failwith ("twitter error: "^Printexc.to_string e)

let twitter ?(host="twitter.com") (user,pass) cmd params is_get =
  let f () =
    Json.parse
      begin if is_get then
	Http.conn host GET ~user:user ~pass:pass cmd params
	  (fun _ ch -> slist "\n" id (read_all ch))
      else
	Http.conn host (POST params) ~user:user ~pass:pass cmd []
	  (fun _ ch -> slist "\n" id (read_all ch))
      end
  in
  catch_twerr f ()

let home_timeline ?since_id ?count acc =
  let params = [("since_id",since_id); ("count", option_map sint count)]
      +> list_filter_map (function
	| (key, Some v) -> Some (key, v)
	| (_, None) -> None)
  in
  twitter acc "/statuses/home_timeline.json" params true
    +> json2timeline

let user_timeline ?since_id ?count acc sname =
  let params = [("since_id",since_id); ("count", option_map sint count);
		("screen_name", Some sname)]
      +> list_filter_map (function
	| (key, Some v) -> Some (key, v)
	| (_, None) -> None)
  in
  twitter acc "/statuses/user_timeline.json" params true
    +> json2timeline

let show acc status_id =
  twitter acc (!%"/statuses/show/%s.json" status_id) [] true

let get_aline acc status_id =
  let prev = Int64.to_string (Int64.pred status_id) in
  home_timeline ~since_id:prev ~count:200 acc
    +> (List.hd $ List.filter (fun tl -> tl.id = Int64.to_string status_id))


let update ?(in_reply_to_status_id) acc text =
  let text = match in_reply_to_status_id with
  | Some id ->
      let tl = get_aline acc (Int64.of_string id) in
      !%"@%s %s" tl.sname text
  | None -> text
  in
  let params = [("in_reply_to_status_id", in_reply_to_status_id);
		("status",Some text)]
      +> list_filter_map (function
	| (key, Some v) -> Some (key, v)
	| (_, None) -> None)
  in
  twitter acc "/statuses/update.json" params false

let retweet acc status_id =
  twitter acc (!%"/statuses/retweet/%s.json" status_id) [] false

let search acc word =
  twitter acc ~host:"search.twitter.com" "/search.json" [("q",word)] true
    +> Json.getf "results"
    +> Json.as_list
    +> List.map (fun j ->
      let d = parse_date @@ Json.as_string @@ Json.getf "created_at" j in
      let sname = Json.as_string @@ Json.getf "from_user" j in
      let text = Json.as_string @@ Json.getf "text" j in
      let id = string_of_float @@ Json.as_float @@ Json.getf"id" j in
      { date=d; sname=sname; id=id; clientname=""; text=text })

