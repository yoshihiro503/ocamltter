(* Oauth_ex.Conf - [app] *)

let oauth_signature_method = `Hmac_sha1
let oauth_callback = None (* Some None (* oob *) *)
let host = "api.twitter.com"
let request_path = "/oauth/request_token"
let access_path = "/oauth/access_token"
let authorize_url = "http://twitter.com/oauth/authorize?oauth_token="
