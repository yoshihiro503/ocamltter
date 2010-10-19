type token = string * string * string
val oauth : token -> Oauth.oauth
val fetch_request_token : unit -> token
val fetch_access_token :
  string -> string -> string -> token
val access :
  Oauth.oauth -> Http.meth -> string -> string -> (string * string) list ->
    (Http.header -> in_channel -> 'a) -> 'a

