let oauth_signature_method = `Hmac_sha1
let oauth_callback = Some None (* oob *)
let host = "www.flickr.com"
let request_path = "/services/oauth/request_token"
let access_path = "/services/oauth/access_token"
let authorize_url = "https://www.flickr.com/services/oauth/authorize?oauth_token="
let app = App.app
