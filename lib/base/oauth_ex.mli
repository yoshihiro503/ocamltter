open Spotlib.Spot
    
module Extra : sig

  include module type of struct include Oauth end

  module Consumer : sig
    type t = { key : string; secret : string; } with conv(ocaml)
    val dummy : t
  end
  
  (* Request and Access tokens are the same format but they should be
     strictly distinguished by their types. *)
  
  module Request_token : sig
    type t = { token : string; secret : string; } with conv(ocaml)
  end
  
  module Access_token : sig
    type t = { token : string; secret : string; } with conv(ocaml)
  end
  
  val oauth : Consumer.t -> Access_token.t -> Oauth.t
  
  module type Conf = sig
  
    val oauth_signature_method : Oauth.signature_method
  
    val oauth_callback : string option option
      (** None : no oauth_callback header
          Some None : oauth_callback=oob  i.e.  Out Of Bound
          Some (Some url)
      *)
  
    val host : string (** "www.flickr.com" *)
  
    val request_path : string (** "/services/oauth/request_token" *)
  
    val access_path : string (** "/services/oauth/access_token" *)
  
    val authorize_url : string
    (** "https://www.flickr.com/services/oauth/authorize?oauth_token=" *)
  
    val app : Consumer.t
  
  end
end

include module type of struct include Extra end

module Make(Conf : Conf) : sig

  include module type of struct include Extra end

  module Conf : Conf

  val fetch_request_token 
    : unit 
    -> (Request_token.t, [> Http.error ]) Result.t
  
  val fetch_access_token 
    : req_token:Request_token.t 
    -> verif:string 
    -> ((string * string) list * Access_token.t, [> Http.error ]) Result.t

  val authorize_cli_interactive 
    : unit -> (string * string) list * Access_token.t
    
  val access 
    : [ `HTTP | `HTTPS ] 
    -> ?oauth_other_params: Http.params 
    -> Oauth.t 
    -> string 
    -> string 
    -> meth:[< `GET of Http.params
            | `POST of Http.params
            | `POST_MULTIPART of (string * [ `CONTENT of string | `FILE of string ]) list ]
    -> (string, [> Http.error ]) Result.t

end

