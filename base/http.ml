open Unix
open Util

let url_encode s =
  let ss = string_foldr (fun c store -> match c with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '.' | '_' | '~' as c -> string1 c :: store
    | c -> ("%" ^ to_hex (int_of_char c)) :: store) s []
  in
  String.concat "" (ss)
    
type header = {
    code : string;
    fields : (string, string) Hashtbl.t
  }

type params = (string * string) list
type meth = GET | POST
let params2string ps =
  String.concat "&" @@ List.map (fun (k,v) -> k^"="^url_encode v) ps

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

let conn ?(port=80) hostname meth ?headers ?user ?pass path ps f =
  let host_entry = Unix.gethostbyname hostname in
  let inet_addr = host_entry.h_addr_list.(0) in
  let sa = Unix.ADDR_INET (inet_addr, port) in
  let auth = match user,pass with
  | Some user, Some pass ->
      !%"Authorization: Basic %s\r\n" (Base64.encode (user ^ ":" ^ pass))
  | _ -> ""
  in
  let hds = match headers with
  | None -> ""
  | Some hds -> slist"\r\n" (fun (k,v) -> k^": "^v) hds ^ "\r\n"
  in
  let msg =
    match meth with
    | GET ->
	let path = if ps<>[] then path ^ "?" ^ params2string ps else path in
	!%"GET %s HTTP/1.0\r\n" path
	^ auth
	^ hds
	^ "Host: " ^ hostname ^ "\r\n"
	^ "\r\n"
    | POST ->
	let s = params2string ps in
	!%"POST %s HTTP/1.0\r\n" path
	^ !%"Content-Length: %d\r\n" (String.length s)
	^ auth
	^ hds
	^ "Host: " ^ hostname ^ "\r\n"
	^ "\r\n"
	^ s
	^ "\r\n"
  in
  (* print_endline msg; *)
  let debug = ref "" in
  let ic, oc = Unix.open_connection sa in
  try
    debug := "write output"; flush oc;
    output_string oc msg; flush oc;
    debug := "read header";
    let header = read_header ic in
    debug := "apply custom function f";
    let x = f header ic in
    debug := "shutdown_connection";
    (try Unix.shutdown_connection ic with e ->
      (*prerr_endline ("Http.conn shutdown error: "^Printexc.to_string e);*)
      ());
    x
  with e ->
    prerr_endline (!%"Http.conn [%s](%s)" (Printexc.to_string e) !debug);
    Unix.shutdown_connection ic;
    raise e

