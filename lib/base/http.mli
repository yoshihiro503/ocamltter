exception Http_error of string

val url_encode  : string -> string
val html_decode : string -> string

type header = { code : string; fields : (string, string) Hashtbl.t; }
type params = (string * string) list
type meth = GET | POST

(** http access *)
val conn :
  ?port:int ->
  string ->
  meth ->
  ?headers: params ->
  string -> params ->
  ?rawdata:string ->
  (header -> in_channel -> 'a) -> 'a

(** https access via cURL *)
val https : 
  string  (** hostname *)
  -> meth  (** GET/POST *)
  -> ?headers: params
  -> string (** path *)
  -> ?rawdata:string (** raw additional post *)
  -> params (** posts *)
  -> [> `Error of [> `Http of int * string ] | `Ok of string ]
