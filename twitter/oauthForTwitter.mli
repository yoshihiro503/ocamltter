val ocamltter_consumer_key : string
val ocamltter_consumer_secret : string
val oauth : string * string * string -> Oauth.oauth
val host : string
val read_lines : in_channel -> string
val parse_http_params : string -> (string * string) list
val read_params : in_channel -> (string * string) list
val assoc : string -> (string * 'a) list -> 'a
val fetch_request_token : unit -> string * string * string
val fetch_access_token :
  string -> string -> string -> string * string * string
