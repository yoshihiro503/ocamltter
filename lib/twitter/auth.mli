module Consumer : sig
  type t = { key : string; secret : string } with conv(ocaml)
  val dummy : t (** a sample data *)
end

(** tokens sent+received between the server and clients *)
module Token : sig
  type t = { token : string; secret : string }
end

module VerifiedToken : sig
  type t = Token.t * string
end

val oauth : Consumer.t -> VerifiedToken.t -> Oauth.t
val fetch_request_token : Consumer.t -> 
  (string (* URL *) * Token.t, 
   [> Http.error ]) Meta_conv.Result.t
val fetch_access_token  : Consumer.t -> VerifiedToken.t -> 
  (string (* username *) * Token.t, 
   [> Http.error ]) Meta_conv.Result.t

val access : 
  [`HTTP | `HTTPS]
  -> ?oauth_other_params: Http.params
  -> ?non_oauth_params: Http.params
  -> Oauth.t 
  -> Http.meth 
  -> string (* host name *)
  -> string (* path *) 
  -> [> `Error of [> Http.error ] 
     |  `Ok of string ]

