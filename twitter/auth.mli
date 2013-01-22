module Consumer : sig
  type t = { key : string; secret : string }
  val ocamltter : t (** Example of consumer key: OCamltter *)
end

(** tokens sent+received between the server and clients *)
module Token : sig
  type t = { token : string; secret : string }
end

module VerifiedToken : sig
  type t = Token.t * string
end

val oauth : Consumer.t -> VerifiedToken.t -> Oauth.t
val fetch_request_token : Consumer.t -> string (* URL *) * Token.t
val fetch_access_token  : Consumer.t -> VerifiedToken.t -> string (* username *) * Token.t

val access :
    Oauth.t -> Http.meth -> string -> string -> (string * string) list ->
    (Http.header -> in_channel -> 'a) -> 'a

val access_https : Oauth.t 
  -> Http.meth 
  -> string 
  -> string 
  -> (string * string) list 
  -> [> `Error of [> `Http of int * string ] | `Ok of string ]
