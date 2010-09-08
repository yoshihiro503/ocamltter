
val fetch_request_token :
  ?http_method:Http.meth ->
  host:string ->
  path:string ->
  ?oauth_version:string ->
  ?oauth_signature_method:[< `Hmac_sha1
                           | `Plaintext
                           | `Rsa_sha1 of 'a & 'b & Cryptokit.RSA.key
                           > `Hmac_sha1 ] ->
  oauth_consumer_key:string ->
  oauth_consumer_secret:string ->
  ?oauth_timestamp:float ->
  ?oauth_nonce:string ->
  ?params:(string * string) list ->
  unit -> (Http.header -> in_channel -> 'c) -> 'c

val fetch_access_token :
  ?http_method:Http.meth ->
  host:string ->
  path:string ->
  ?oauth_version:string ->
  ?oauth_signature_method:[< `Hmac_sha1
                           | `Plaintext
                           | `Rsa_sha1 of 'a & 'b & Cryptokit.RSA.key
                           > `Hmac_sha1 ] ->
  oauth_consumer_key:string ->
  oauth_consumer_secret:string ->
  oauth_token:string ->
  oauth_token_secret:string ->
  verif:string ->
  ?oauth_timestamp:float ->
  ?oauth_nonce:string -> unit -> (Http.header -> in_channel -> 'c) -> 'c

val access_resource :
  ?http_method:Http.meth ->
  host:string ->
  path:string ->
  ?oauth_version:string ->
  ?oauth_signature_method:[< `Hmac_sha1
                           | `Plaintext
                           | `Rsa_sha1 of 'a & 'b & Cryptokit.RSA.key
                           > `Hmac_sha1 ] ->
  oauth_consumer_key:string ->
  oauth_consumer_secret:string ->
  oauth_token:string ->
  oauth_token_secret:string ->
  verif:'c ->
  ?oauth_timestamp:float ->
  ?oauth_nonce:string ->
  ?params:(string * string) list ->
  ?body:'d -> unit -> (Http.header -> in_channel -> 'e) -> 'e

type oauth = {
  consumer_key : string;
  consumer_secret : string;
  access_token : string;
  access_token_secret : string;
  verif : string;
}

val access :
  oauth -> Http.meth -> string -> string -> (string * string) list -> string
