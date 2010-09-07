open Util
open Http
open Oauth

let ocamltter_consumer_key = "vS0nKAS6ieWL76zZaQgF4A"
let ocamltter_consumer_secret = "XHa1ZiPcNRsYKw4mdIv8wHUiNulpBFxKT1ntXXuJgo"

let oauth(tok, sec, verif) = {
  Oauth.consumer_key = ocamltter_consumer_key;
  Oauth.consumer_secret = ocamltter_consumer_secret;
  Oauth.access_token = tok;
  Oauth.access_token_secret = sec;
  Oauth.verif = verif
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

let fetch_request_token () = 
      Oauth.fetch_request_token 
        ~host:host
        ~path:"/oauth/request_token"
        ~oauth_consumer_key:ocamltter_consumer_key
        ~oauth_consumer_secret:ocamltter_consumer_secret
        ()
    (fun _ ch ->
  let res = read_params ch in
  let token, secret = assoc "oauth_token" res, assoc "oauth_token_secret" res in
    "http://twitter.com/oauth/authorize?oauth_token="^token, token, secret)

let fetch_access_token req_token req_secret verif = 
  fetch_access_token 
    ~http_method:GET
    ~host:host
    ~path:"/oauth/access_token"
    ~oauth_consumer_key:ocamltter_consumer_key
    ~oauth_consumer_secret:ocamltter_consumer_secret
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
      in user, acc_token, acc_secret
    )
