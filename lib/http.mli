open Spotlib.Spot

exception Http_error of string

val url_encode  : string -> string
val html_decode : string -> string
val html_encode : string -> string

type header = { code : string; fields : (string, string) Hashtbl.t; }
type headers = (string * string) list
type params = (string * string) list
type params2 = (string * [ `String of string
                         | `File   of string (** file contents *) ]) list
type meth = [ `GET | `POST ]
val string_of_meth : meth -> string

type error = 
  [ `Http of int * string  (** HTTP status other than 200 *)
  ]

val string_of_error : error -> string

val by_cohttp_gen :
  ?proto:[ `HTTP | `HTTPS ]
  -> string (** hostname *)
  -> ?port:int
  -> string (** path *)
  -> headers:(string * string) list
  -> [ `GET of params (** GET *)
     | `POST of params (** POST *)
     | `POST_MULTIPART of params2 (** POST by multipart *)
     ]
  -> ((string, [> error]) Result.t) Lwt.t

val by_cohttp :
  ?proto:[ `HTTP | `HTTPS ]
  -> string (** hostname *)
  -> ?port:int
  -> string (** path *)
  -> headers:(string * string) list
  -> [ `GET of params (** GET *)
     | `POST of params (** POST *)
     | `POST_MULTIPART of params2 (** POST by multipart *)
     ]
  -> (string, [> error]) Result.t
  
