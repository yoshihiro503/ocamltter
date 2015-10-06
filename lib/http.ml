open Unix
open Spotlib.Spot
open Util

exception Http_error of string

let url_encode' s =
  let ss = string_foldr (fun c store -> match c with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '.' | '_' | '~' as c -> String.make1 c :: store
    | c -> ("%" ^ to_hex (int_of_char c)) :: store) s []
  in
  String.concat "" (ss)

let url_encode s =
  let b = Buffer.create (String.length s * 2) in
  String.iter (function 
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '.' | '_' | '~' as c -> 
        Buffer.add_char b c
    | c -> 
        Buffer.add_char b '%';
        Buffer.add_string b @@ to_hex (int_of_char c)) s;
  Buffer.contents b

let url_encode s =
  let e1 = url_encode' s in
  let e2 = url_encode s in
  assert (e1 = e2);
  e2

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

let html_encode s =
  let rec aux store = function
    | '&'::cs -> aux ('&'::'a'::'m'::'p'::';'::store) cs
    | '\"'::cs -> aux ('&'::'q'::'u'::'o'::'t'::';'::store) cs
    | '<'::cs -> aux ('&'::'l'::'t'::';'::store) cs
    | '>'::cs -> aux ('&'::'g'::'t'::';'::store) cs
    | c :: cs -> aux (c::store) cs
    | [] -> List.rev store
  in
  string_of_chars @@ aux [] @@ chars_of_string s
    
type header = {
    code : string;
    fields : (string, string) Hashtbl.t
  }

type headers = (string * string) list
type params = (string * string) list
type params2 = (string * [ `String of string
                         | `File   of string (** file contents *) ]) list

type meth = [ `GET | `POST ]

let string_of_meth = function
  | `GET ->  "GET"
  | `POST -> "POST"

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

let conn ?(port=80) hostname meth ?headers  path ps ?(rawpost="") f =
  let host_entry = Unix.gethostbyname hostname in
  let inet_addr = host_entry.h_addr_list.(0) in
  let sa = Unix.ADDR_INET (inet_addr, port) in
  let hds = match headers with
    | None -> ""
    | Some hds -> slist"\r\n" (fun (k,v) -> k^": "^v) hds ^ "\r\n"
  in
  let msg =
    match meth with
    | `GET ->
        assert (rawpost="");
	let path = if ps<>[] then path ^ "?" ^ params2string ps else path in
	!%"GET %s HTTP/1.0\r\n" path
	^ hds
	^ "Host: " ^ hostname ^ "\r\n"
	^ "\r\n"
    | `POST ->
	let s = params2string ps ^ rawpost in
	!%"POST %s HTTP/1.0\r\n" path
	^ "Host: " ^ hostname ^ "\r\n"
	^ !%"Content-Length: %d\r\n" (String.length s)
	^ hds
	^ "\r\n"
	^ s
	^ "\r\n"
  in
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

type error = 
  [ `Http of int * string  (** HTTP status other than 200 *)
  | `Curl of Curl.curlCode * int * string (** libcURL error *)
  ]

let string_of_error = function
  | `Http (n, s) -> !%"Http error %d: %s" n s
  | `Curl (cc, n, s) -> !%"Curl (%s) %d: %s" (Curl.strerror cc) n s

let by_curl_gen ?(proto=`HTTPS) hostname ?port path ~headers meth_params =
  let open Curl in
  let h = new Curl.handle in
  (* h#set_verbose true; *)
  let proto_string = match proto with `HTTP -> "http" | `HTTPS -> "https" in
  let url = !% "%s://%s%s%s" 
    proto_string 
    hostname 
    (match port with None -> "" | Some p -> !% ":%d" p) 
    path
  in
  let headers = ("Host", hostname) :: headers in
  (* DEBUG List.iter (fun (k,v) -> Printf.eprintf "%s: %s\n%!" k v) headers; *)
  begin match meth_params with
  | `GET params ->
      let url = if params <> [] then url ^ "?" ^ params2string params else url in
      h#set_url url;
      h#set_post false;
      h#set_httpheader (List.map (fun (k,v) -> !% "%s: %s" k v) headers);
  | `POST params ->
      h#set_url url;
      h#set_post true;
      let s = params2string params in
      h#set_postfields s;
        (* set_postfields of OCurl 0.5.3 has a bug. 
           We need explicit set_postfieldsize to workaround it.
        *)
      h#set_postfieldsize (String.length s);
      h#set_httpheader (List.map (fun (k,v) -> !% "%s: %s" k v) headers);
  | `POST_MULTIPART params ->
      h#set_url url;
      h#set_post true;
      h#set_httppost 
        (List.map (function
          | (k, `File path) -> CURLFORM_FILE (k, path, DEFAULT)
          | (k, `String s) -> CURLFORM_CONTENT (k, s, DEFAULT)) params);
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

let by_curl ?proto hostname ?port path ~headers meth_params =
  try 
    by_curl_gen ?proto hostname ?port path ~headers meth_params
  with
  | Curl.CurlException (curlCode, int, mes) ->
      `Error (`Curl (curlCode, int, mes))
