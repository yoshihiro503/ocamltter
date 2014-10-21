module Make(A : sig
  val app : Oauth_ex.Consumer.t
end) = struct

  module Conf = struct
    let oauth_signature_method = `Hmac_sha1
    let oauth_callback = None (* Some None (* oob *) *)
    let host = "api.twitter.com"
    let request_path = "/oauth/request_token"
    let access_path = "/oauth/access_token"
    let authorize_url = "http://twitter.com/oauth/authorize?oauth_token="
    let app = A.app
  end

  module Oauth = Oauth_ex.Make(Conf)

end

