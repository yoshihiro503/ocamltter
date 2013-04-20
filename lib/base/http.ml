open Unix
open Util

exception Http_error of string

let url_encode s =
  let ss = string_foldr (fun c store -> match c with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '.' | '_' | '~' as c -> string1 c :: store
    | c -> ("%" ^ to_hex (int_of_char c)) :: store) s []
  in
  String.concat "" (ss)

let html_decode s =
  let rec aux store = function
    | '&'::'a'::'m'::'p'::';'::cs -> aux ('&'::store) cs
    | '&'::'q'::'u'::'o'::'t'::';'::cs -> aux ('\"'::store) cs
    | '&'::'l'::'t'::';'::cs -> aux ('<'::store) cs
    | '&'::'g'::'t'::';'::cs -> aux ('>'::store) cs
    | c :: cs -> aux (c::store) cs
    | [] -> List.rev store
  in
  string_of_chars @@ aux [] @@ chars_of_string s
    
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

(* CR jfuruse: unused 
val read_all_and_count : in_channel -> string * int
let read_all_and_count ic =
  let rec loop store count =
    try loop (input_char ic :: store) (succ count) with
    | End_of_file -> (slist "" string1 (List.rev store), count)
  in
  loop [] 0
*)

let conn ?(port=80) hostname meth ?headers  path ps ?(rawdata="") f =
  let host_entry = Unix.gethostbyname hostname in
  let inet_addr = host_entry.h_addr_list.(0) in
  let sa = Unix.ADDR_INET (inet_addr, port) in
  let hds = match headers with
  | None -> ""
  | Some hds -> slist"\r\n" (fun (k,v) -> k^": "^v) hds ^ "\r\n"
  in
  let msg =
    match meth with
    | GET ->
	let path = if ps<>[] then path ^ "?" ^ params2string ps else path in
	!%"GET %s HTTP/1.0\r\n" path
	^ hds
	^ "Host: " ^ hostname ^ "\r\n"
	^ "\r\n"
    | POST ->
	let s = params2string ps ^ rawdata in
	!%"POST %s HTTP/1.0\r\n" path
	^ "Host: " ^ hostname ^ "\r\n"
	^ !%"Content-Length: %d\r\n" (String.length s)
	^ hds
	^ "\r\n"
	^ s
	^ "\r\n"
  in
prerr_endline msg;
  (*print_endline msg;*)
  let debug = ref "" in
  let ic, oc = Unix.open_connection sa in
  let close () =
    ignore @@ maybe Unix.shutdown_connection ic;
    ignore @@ maybe close_in  ic;
    ignore @@ maybe close_out oc;
  in
  try
    debug := "writing output..."; flush oc;
    output_string oc msg; flush oc;
    debug := "reading header...";
    let header = read_header ic in
    debug := "appling custom function f...";
    let x = f header ic in
    debug := "closing...";
    close ();
    x
  with e ->
    close ();
    raise @@ Http_error (!%"[%s] -> %s\n%s" !debug (Printexc.to_string e) msg)


let () = Curl.global_init Curl.CURLINIT_GLOBALALL

let https hostname meth ?(headers=[]) path ?(rawdata="") ps =
  let h = new Curl.handle in
  (* h#set_verbose true; *)
  let url = !% "https://%s%s" hostname path in
  let headers = ("Host", hostname) :: headers in
  (* List.iter (fun (k,v) -> Printf.eprintf "%s: %s\n%!" k v) headers; *)
  begin match meth with
    | GET ->
      let url = if ps<>[] then url ^ "?" ^ params2string ps else url in
        h#set_url url;
        h#set_post false;
        h#set_httpheader (List.map (fun (k,v) -> !% "%s: %s" k v) headers);
    | POST ->
        h#set_url url;
        h#set_post true;
        let s = params2string ps ^ rawdata in
        h#set_postfields s;
        (* set_postfields of OCurl 0.5.3 has a bug. 
           We need explicit set_postfieldsize to workaround it.
        *)
        h#set_postfieldsize (String.length s);
        h#set_httpheader (List.map (fun (k,v) -> !% "%s: %s" k v) headers);
  end;
  
  let buf = Buffer.create 100 in
  assert (h#get_cookielist = []);
  h#set_writefunction (fun s -> Buffer.add_string buf s; String.length s);
  h#perform;
  let code = h#get_httpcode in
  h#cleanup; (* Need to flush out cookies *)
  let ok200 = function
    | 200, v -> `Ok v
    | n, mes -> `Error (`Http (n, mes))
  in	
  ok200 (code, Buffer.contents buf)
