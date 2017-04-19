open Spotlib.Spot

exception Http_error of string

val url_encode  : string -> string
val html_decode : string -> string
val html_encode : string -> string

type header = { code : string; fields : (string, string) Hashtbl.t; }
type headers = (string * string) list
type params = (string * string) list
type params2 = (string * [ `String of string
                         | `File   of string (*+ file contents *) ]) list
type meth = [ `GET | `POST ]
val string_of_meth : meth -> string

(** http access *)
val conn :
  ?port:int 
  -> string          (*+ hostname *)
  -> meth            (*+ GET/POST *)
  -> ?headers: params 
  -> string          (*+ path *)
  -> params          (*+ posts *)
  -> ?rawpost:string (*+ raw additional post *)
  -> (header -> in_channel -> 'a) 
  -> 'a

type error = 
  [ `Http of int * string  (*+ HTTP status other than 200 *)
  | `Curl of Curl.curlCode * int * string (*+ libcURL error *)
  ]

val string_of_error : error -> string

(** http/https access via cURL *)
val by_curl : 
  ?proto : [`HTTP | `HTTPS ] (*+ protocol. The default is HTTPS *)
  -> string            (*+ hostname *)
  -> ?port: int        (*+ port: the default is the default port of the protocol *)
  -> string            (*+ path *)
  -> headers: headers
  -> [ `GET of params (*+ GET *)
     | `POST of params (*+ POST *)
     | `POST_MULTIPART of params2 (*+ POST by multipart *)
     ]
  -> (string, [> error]) result
