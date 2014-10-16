open Spotlib.Spot

exception Http_error of string

val url_encode  : string -> string
val html_decode : string -> string

type header = { code : string; fields : (string, string) Hashtbl.t; }
type params = (string * string) list

type meth = [ `GET | `POST ]
val string_of_meth : meth -> string

(** http access *)
val conn :
  ?port:int 
  -> string          (** hostname *)
  -> meth            (** GET/POST *)
  -> ?headers: params 
  -> string          (** path *)
  -> params          (** posts *)
  -> ?rawpost:string (** raw additional post *)
  -> (header -> in_channel -> 'a) 
  -> 'a

type error = 
  [ `Http of int * string  (** HTTP status other than 200 *)
  | `Curl of Curl.curlCode * int * string (** libcURL error *)
  ]

val string_of_error : error -> string

(* CR jfuruse: no way to specify the port *)
(** http/https access via cURL *)
val by_curl : 
  ?handle_tweak: (Curl.handle -> unit) (** Final tweak func of Curl handler *)
  -> [`HTTP | `HTTPS ] (** protocol *)
  -> string            (** hostname *)
  -> ?port: int        (** port *)
  -> string            (** path *)
  -> headers: params
  -> [ `GET of params
     | `POST of params
     | `POST2 of (string * [ `CONTENT of string | `FILE of string ]) list
     ] (** GET/POST with parameters.
           POST2 sends parameters using multi-part. 
           It can also ask cURL to send files by their file names.
       *)
  -> (string, error) Result.t
