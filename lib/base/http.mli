exception Http_error of string

val url_encode  : string -> string
val html_decode : string -> string

type header = { code : string; fields : (string, string) Hashtbl.t; }
type params = (string * string) list

type meth = [ `GET | `POST | `POST2 ]
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
(** http access via cURL *)
val by_curl : 
  ?handle_tweak: (Curl.handle -> unit) (** Final tweak func of Curl handler *)
  -> meth              (** GET/POST *)
  -> [`HTTP | `HTTPS ] (** protocol *)
  -> string            (** hostname *)
  -> ?port: int        (** port *)
  -> string            (** path *)
  -> params: params    (** get/post parameters *)
  -> headers: params
  -> [> `Error of [> error ]
     |  `Ok of string ]
