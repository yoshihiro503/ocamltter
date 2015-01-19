open OCamltter_oauth

module Oauth : module type of struct include Oauth_ex.Make(Conf) end

val load_auth     : string -> Oauth.Access_token.t
val get_acc_token : string -> Oauth.Access_token.t
val get_oauth     : string -> Oauth.t
