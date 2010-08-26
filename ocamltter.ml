open Util
open Util.Date
open TwitterApi
open Http

let oauth_acc : (string * string * string) option ref = ref None
let conffile = ".ocamltter"

let authorize () = 
  let url, req_tok, req_sec = TwitterApi.fetch_request_token () in
  print_endline ("Please grant access to ocamltter ant get a PIN at :\n\t" ^ url);
  print_endline "Give me a PIN : ";
  let verif = read_line () in
  let username, acc_tok, acc_sec = TwitterApi.fetch_access_token req_tok req_sec verif in
  oauth_acc := Some (acc_tok, acc_sec, verif);
  print_string ("Grant Success! Hello, @"^username^" !");
  open_out_with conffile (fun ch -> output_string ch (acc_tok^"\n"^acc_sec^"\n"^verif^"\n"));
  (acc_tok, acc_sec, verif)

module Cash = struct
  type t = (string, timeline) Hashtbl.t
  let init () : t = Hashtbl.create 100
  let is_new (cash : t) (x : timeline) =
    not (Hashtbl.mem cash x.id)
  let add cash (p: timeline) = Hashtbl.add cash p.id p
end

let load () = open_in_with conffile (fun ch -> let tok=input_line ch in let sec=input_line ch in let verif=input_line ch in
  let acc=(tok,sec,verif) in oauth_acc := Some acc; acc)

let oauth () =
  match !oauth_acc with
  | None -> (try load () with _ -> authorize ())
  | Some a -> a

let setup () = ignore(oauth())


let get_timeline ?(c=20) () =
try
    let tl1 =
      List.filter Config.filter @@ TwitterApi.home_timeline ~count:c (oauth())
    in
    let tl2 =
(*      list_concatmap (TwitterApi.search (oauth())) Config.watching_words*)
      []
    in
    let post_comp p1 p2 = compare p1.date p2.date in
    List.sort post_comp @@ (tl1@tl2)
with
| e ->
    prerr_endline (!%"Ocamltter.print_tl [%s]" (Printexc.to_string e));
    raise e

let print_timeline tl =
  List.iter print_endline @@ List.map show_tl tl

let lc count = get_timeline ~c:count ()
let lu user = TwitterApi.user_timeline (oauth()) user

let l ?(c=20) ?u () : TwitterApi.timeline list =
  match u with
  | None -> lc c
  | Some user -> lu user

let u text =
  ignore @@ TwitterApi.update (oauth()) text

let rt status_id =
  ignore @@ TwitterApi.retweet (oauth()) (Int64.to_string status_id)

let re status_id text =
  ignore @@ TwitterApi.update ~in_reply_to_status_id:(Int64.to_string status_id)
    (oauth()) text

let qt status_id text =
  let tl = get_aline (oauth()) status_id in
  u (!%"%s QT @%s: %s" text tl.sname tl.text)
(*
let s word = TwitterApi.search (oauth()) word
  *)
let help =
"commands:
  l()                list timeline
  lc N               list timeline(N lines)
  lu \"NAME\"          list NAME's timeline
  u \"TEXT\"           post a new message
  re ID \"TEXT\"       reply to ID
  rt ID              retweet ID
  qt ID \"TEXT\"       qt ID
  s \"WORD\"           search by a WORD
  let CMD = ...      define a your own command CMD
  setup()            (re)authorize ocamltter
  help               print this help
"

let start_polling () =
  let cash = Cash.init () in
  let rec loop () =
    begin try
      let tl = List.filter (Cash.is_new cash) (get_timeline ~c:200 ()) in
      List.iter (Cash.add cash) tl;
      print_timeline tl
    with e -> print_endline (Printexc.to_string e)
    end;
    Thread.delay Config.coffee_break;
    loop ()
  in
  let t = Thread.create loop in
  t ()
