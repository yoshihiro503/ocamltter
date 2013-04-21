type signature_method = [ `Hmac_sha1
                        | `Plaintext
                        | `Rsa_sha1 of Cryptokit.RSA.key 
                        ]

val fetch_request_token : 
  ?http_method:Http.meth 
  -> host:string 
  -> path:string 

  -> ?oauth_version:string 
  -> ?oauth_signature_method:signature_method 
  -> ?oauth_timestamp:float 
  -> ?oauth_nonce:string 
  -> oauth_consumer_key:string 
  -> oauth_consumer_secret:string 

  -> unit
  -> [> `Error of [> `Http of int * string ] | `Ok of string ]

val fetch_access_token : 
  verif:string 
  -> oauth_token:string 
  -> oauth_token_secret:string 

  -> ?http_method:Http.meth 
  -> host:string 
  -> path:string 

  -> ?oauth_version:string 
  -> ?oauth_signature_method:signature_method 
  -> ?oauth_timestamp:float 
  -> ?oauth_nonce:string 
  -> oauth_consumer_key:string 
  -> oauth_consumer_secret:string 

  -> unit
  -> [> `Error of [> `Http of int * string ] | `Ok of string ]

type t = {
  consumer_key        : string;
  consumer_secret     : string;
  access_token        : string;
  access_token_secret : string;
} with conv(ocaml)

val access :
  [ `HTTP | `HTTPS ]
  -> t 
  -> Http.meth 
  -> string (** host *)
  -> string (** path *) 
  -> (string * string) list (** params *)
  -> [> `Error of [> `Http of int * string ] | `Ok of string ]
