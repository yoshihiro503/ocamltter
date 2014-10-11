type signature_method = [ `Hmac_sha1
                        | `Plaintext
                        | `Rsa_sha1 of Cryptokit.RSA.key 
                        ]

val fetch_request_token : 
  ?http_method:Http.meth 
  -> ?oauth_other_params: Http.params  (** e.g. oauth_callback *)
  -> ?handle_tweak:(Curl.handle -> unit)
  -> host:string 
  -> ?port:int
  -> path:string 

  -> ?oauth_version:string 
  -> ?oauth_signature_method:signature_method 
  -> ?oauth_timestamp:float 
  -> ?oauth_nonce:string 
  -> oauth_consumer_key:string 
  -> oauth_consumer_secret:string 

  -> unit
  -> [> `Error of [> Http.error ] 
     |  `Ok of string ]

val fetch_access_token : 
  verif:string 
  -> oauth_token:string 
  -> oauth_token_secret:string 

  -> ?http_method:Http.meth 

  -> ?handle_tweak:(Curl.handle -> unit)
  -> host:string 
  -> ?port:int
  -> path:string 

  -> ?oauth_version:string 
  -> ?oauth_signature_method:signature_method 
  -> ?oauth_timestamp:float 
  -> ?oauth_nonce:string 
  -> oauth_consumer_key:string 
  -> oauth_consumer_secret:string 

  -> unit
  -> [> `Error of [> Http.error ] 
     |  `Ok of string ]

type t = {
  consumer_key        : string;
  consumer_secret     : string;
  access_token        : string;
  access_token_secret : string;
} with conv(ocaml)

val access :
  [ `HTTP | `HTTPS ]
  -> ?oauth_other_params: Http.params
  -> ?non_oauth_params: Http.params
  -> t 
  -> Http.meth 
  -> string (** host *)
  -> string (** path *) 
  -> [> `Error of [> Http.error ] 
     |  `Ok of string ]

val access_post2 : [< `HTTP | `HTTPS ] ->
                        ?oauth_other_params:(string * string) list ->
                        ?non_oauth_params:(string *
                                           [< `CONTENT of string
                                            | `FILE of string
                                            > `CONTENT ])
                                          list ->
                        t ->
                        [< `POST2 ] ->
                        string ->
                        string ->
                        [> `Error of [> Http.error ]
                         | `Ok of string ]
