open Unix
open Util

type header = {
    code : string;
    fields : (string, string) Hashtbl.t
  }

type meth = GET | POST of string

let read_header in_ch =
  let rec iter store =
    match input_line in_ch with
    | "" | "\r" -> store
    | line -> iter (line :: store)
  in
  let _lines = iter [] in
  {code=""; fields=Hashtbl.create 30}

let read_all_and_count ic =
  let rec loop store count =
    try loop (input_char ic :: store) (succ count) with
    | End_of_file -> (slist "" string1 (List.rev store), count)
  in
  loop [] 0

let split2 r s : string * string =
  match Str.split (Str.regexp r) s with
  | [] -> "",""
  | [s1] -> s1, ""
  | s1::ss -> s1, String.concat r ss

let conn meth ?user ?pass url f =
  let hostname, path = split2 "/" url in
  let host_entry = Unix.gethostbyname hostname in
  let inet_addr = host_entry.h_addr_list.(0) in
  let sa = Unix.ADDR_INET (inet_addr, 80) in
  let auth = match user,pass with
  | Some user, Some pass ->
      !%"Authorization: Basic %s\r\n" (Base64.encode (user ^ ":" ^ pass))
  | _ -> ""
  in
  let msg =
    match meth with
    | GET ->
	!%"GET /%s HTTP/1.0\r\n" path
	^ auth
	^ "\r\n"
    | POST s ->
	!%"POST /%s HTTP/1.0\r\n" path
	^ !%"Content-Length: %d\r\n" (String.length s)
	^ auth
	^ "\r\n"
	^ s
	^ "\r\n"
  in
  let debug = ref "" in
  let ic, oc = Unix.open_connection sa in
  try
    output_string oc msg; flush oc;
    debug := "send";
    let header = read_header ic in
    debug := "read_header";
    let x = f header ic in
    debug := "apply f";
    Unix.shutdown_connection ic;
    debug := "shdowned";
    x
  with e ->
    prerr_endline (!%"Http.conn [%s](%s)" (Printexc.to_string e) !debug);
    Unix.shutdown_connection ic;
    raise e

let usage =
"Usage: ./wget <hostname>
Examples:
  twitter.com/statuses/user_timeline.json/?screen_name=yoshihiro503
"

let url_encode s =
  let ss = string_foldr (fun c store -> match c with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' as c -> string1 c :: store
    | c -> ("%" ^ to_hex (int_of_char c)) :: store) s []
  in
  String.concat "" (ss)
    
