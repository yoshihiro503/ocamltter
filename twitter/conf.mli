(** Configuration of a service
  
    This is equivalent with [Oauth_ex.Conf] but without [app]. *)

val oauth_signature_method : OCamltter_oauth.Oauth.signature_method
val oauth_callback : string option option
val host : string
val request_path : string
val access_path : string
val authorize_url : string
