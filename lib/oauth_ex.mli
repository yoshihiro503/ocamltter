open Spotlib.Spot
    
module Extra : sig

  include module type of struct include Oauth end
  (* It is an extension of Oauth *)
     
  module Consumer : sig
    type t = { key : string; secret : string; } with conv(ocaml)
    val dummy : t

    (** The type of Consumer (i.e. App). Normally you get this from
        the service provider (Twitter/Flickr) by registering a new app.
    *)
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
  (** The key to APIs is a combination of [Consumer.t] and [Access_token.t] *)

  (** Configuration of a service *)  
  module type Conf = sig
  
    val oauth_signature_method : Oauth.signature_method
    (** Which signature method is used *)
  
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
  (* It is an extension of Extra, which is an extension of Oauth *)

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
  (** Starts CLI OAuth authorization.
      It asks the user to access an URL of the service to grant the app
      to access the service using the user credential. Once approved, 
      the service should return a verification key string. The user is
      prompted to input the verification string. Using it the function
      finishes the OAuth autothorizatoin and returns the access token
      [Access_token.t] and other information as [(string * string) list].
  *)

  val access 
    : ?proto: [ `HTTP | `HTTPS ] (** default is `HTTPS *)
    -> host: string (** host *)
    -> ?port: int   (** port *)
    -> path:string  (** path *) 
    -> meth:[< `GET of Http.params
            | `POST of Http.params
            | `POST_MULTIPART of Http.params2 ]
       (** These parameters are outside of OAuth signature creation *)
    -> oauth_other_params: Http.params
       (** These parameters are included in the targets for OAuth signature creation *)
    -> t (** Auth *)
    -> (string, [> Http.error]) Result.t 

end


