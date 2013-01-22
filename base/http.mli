exception Http_error of string
val url_encode : string -> string
val html_decode : string -> string
type header = { code : string; fields : (string, string) Hashtbl.t; }
type params = (string * string) list
type meth = GET | POST
val params2string : (string * string) list -> string
val read_header : in_channel -> header
val read_all_and_count : in_channel -> string * int
val conn :
  ?port:int ->
  string ->
  meth ->
  ?headers:(string * string) list ->
  string -> (string * string) list ->
  ?rawdata:string ->
    (header -> in_channel -> 'a) -> 'a

(** https access via cURL *)
val https : 
  string  (** hostname *)
  -> meth  (** GET/POST *)
  -> ?headers:(string * string) list 
  -> string (** path *)
  -> ?rawdata:string (** raw additional post *)
  -> (string * string) list (** posts *)
  -> [> `Error of [> `Http of int * string ] | `Ok of string ]
