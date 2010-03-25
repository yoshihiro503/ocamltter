open Util
open Http
open Json

type timeline = {
    date: int * int;
    sname: string;
    id: int64;
    text: string
  }

let parse_date s =
  let h = String.sub s 11 2 in
  let m = String.sub s 14 2 in
  (int_of_string h+9, int_of_string m)

let date_lt (h1,m1) (h2,m2) =
  (h1 < h2) || (h1=h2 && m1<m2)

let show tl =
  let h,m = tl.date in
  !%"%d:%d %s: %s" h m tl.sname tl.text

let get_timeline j =
  let post j =
    let date =
      Json.getf "created_at" j +> Json.as_string +> parse_date
    in
    let text =
      Json.getf "text" j +> Json.as_string
    in
    let id =
      Json.getf "id" j +> Json.as_float +> Int64.of_float
    in
    let sname =
      Json.getf "user" j +> Json.getf "screen_name" +> Json.as_string
    in
    {date=date; sname=sname; text=text; id=id}
  in
  Json.as_list j +> List.map post

let twitter f =
  try f () with
  | e -> failwith ("twitter error: "^Printexc.to_string e)
(*  | JSON_NotObject j ->
  | JSON_InvalidField s ->
  | JSON_CastErr s ->*)

let home_timeline (user, pass) =
  twitter begin fun () ->
    let j =
      Http.conn GET ~user:user ~pass:pass
	"twitter.com/statuses/home_timeline.json"
	(fun _ ch -> slist "\n" id (read_all ch))
    in
    Json.parse j +> get_timeline
  end

let update (user, pass) text =
  twitter begin fun () ->
    let r =
      let arg =
	!%"status=%s" (Http.url_encode text)
      in
      Http.conn (POST arg) ~user:user ~pass:pass
	"twitter.com/statuses/update.json"
	(fun _ ch -> slist "\n" id (read_all ch))
    in
    Json.parse r
  end
