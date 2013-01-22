open Util
open Http
open Oauth

module Consumer = struct
  type t = { key : string; secret : string }
  let ocamltter = {
    key    = "vS0nKAS6ieWL76zZaQgF4A";
    secret = "XHa1ZiPcNRsYKw4mdIv8wHUiNulpBFxKT1ntXXuJgo";
  }
end

module Token = struct
  type t = { token : string; secret : string }
end

module VerifiedToken = struct
  type t = Token.t * string
end

let oauth app (tkn, verif) = {
  Oauth.consumer_key  = app.Consumer.key;
  consumer_secret     = app.Consumer.secret;
  access_token        = tkn.Token.token;
  access_token_secret = tkn.Token.secret;
  verif               = verif;
}

let host = "api.twitter.com"

let read_lines ch = slist "\n" id (read_all ch)
let parse_http_params s = 
  Str.split(Str.regexp"&") s |> 
  List.map (fun s -> 
    let l = Str.split (Str.regexp "=") s 
    in try List.hd l, List.hd (List.tl l) with Failure _ -> raise (Failure ("can't parse"^s)))
let read_params ch = parse_http_params (read_lines ch)
let assoc key dic = try List.assoc key dic with Not_found -> raise (Failure (key ^ " not found"))

let fetch_request_token app = 
  fetch_request_token 
    ~host:host
    ~path:"/oauth/request_token"
    ~oauth_consumer_key:    app.Consumer.key
    ~oauth_consumer_secret: app.Consumer.secret
    ()
    (fun _ ch ->
      let res = read_params ch in
      let token, secret = assoc "oauth_token" res, assoc "oauth_token_secret" res in
      "http://twitter.com/oauth/authorize?oauth_token="^token,
      { Token.token; secret })

let fetch_access_token app ({Token.token= req_token; secret= req_secret}, verif) = 
  fetch_access_token 
    ~http_method:GET
    ~host:host
    ~path:"/oauth/access_token"
    ~oauth_consumer_key: app.Consumer.key
    ~oauth_consumer_secret: app.Consumer.secret
    ~oauth_token:req_token
    ~oauth_token_secret:req_secret
    ~verif:verif
    ()
    (fun _ ch -> 
      let res = read_params ch in
      let acc_token, acc_secret, user = 
        assoc "oauth_token" res, 
        assoc "oauth_token_secret" res,
        assoc "screen_name" res
      in user, { Token.token= acc_token; secret= acc_secret }
    )

let access = Oauth.access
let access_https = Oauth.access_https
