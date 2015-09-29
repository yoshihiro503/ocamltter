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

type error = 
  [ `Http of int * string  (** HTTP status other than 200 *)
  ]

let string_of_error = function
  | `Http (n, s) -> !%"Http error %d: %s" n s

let by_cohttp_gen ?(proto=`HTTPS) hostname ?port path ~headers meth_params =
  let open Cohttp in
  let open Cohttp_lwt_unix in      
  let headers = Header.of_list (("Host", hostname) :: headers) in
  let query params = match params with
    | [] -> None
    | _ -> Some (List.map (fun (k,v) -> (k,[v])) params)
  in
  let uri params =
    let scheme = match proto with `HTTP -> "http" | `HTTPS -> "https" in
    Uri.make ~scheme ~host:hostname ?port ~path ?query:(query params) ()
  in
  let lwt = match meth_params with
    | `GET params -> Client.get ~headers (uri params)
    | `POST params ->
        Client.post_form ~headers ~params:(List.map (fun (k,v) -> (k,[v])) params) (uri [])
    | `POST_MULTIPART params ->
        Client.post_form ~headers
          ~params:(List.map (fun (k,v) -> match v with
            | `String s -> (k, [s])
            | `File path -> (k, [ Result.from_Ok & File.to_string path ])) params)
          (uri [])
  in
  let open Lwt in
  lwt >>= fun (resp, body) ->
  let open Response in
  match Code.code_of_status resp.status with
  | 200 ->
      Cohttp_lwt_body.to_string body >>= fun mes ->
      return & `Ok mes
  | code ->
      Cohttp_lwt_body.to_string body >>= fun mes ->
      return & `Error (`Http (code, mes))

let by_cohttp ?proto hostname ?port path ~headers meth_params =
  Lwt_main.run & by_cohttp_gen ?proto hostname ?port path ~headers meth_params
        
