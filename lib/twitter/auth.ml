open Spotlib.Spot
open Spotlib.Result.Open (* Monads are Result *)
open Util
open Oauth
open Ocaml_conv

module Consumer = struct
  type t = { key : string; secret : string } with conv(ocaml)
  let dummy = { key = "Base64EncodedDataHereX";
                secret = "AnotherBase64EncodedDataHereAnotherBase64E"; }
end

module Token = struct
  type t = { token : string; secret : string }
end

module VerifiedToken = struct
  type t = Token.t * string
end

let oauth app (tkn, _verif) = {
  Oauth.consumer_key  = app.Consumer.key;
  consumer_secret     = app.Consumer.secret;
  access_token        = tkn.Token.token;
  access_token_secret = tkn.Token.secret;
}

let host = "api.twitter.com"

let parse_http_params s = 
  String.split (function '&' -> true | _ -> false) s |> 
  List.map (fun s -> 
    let l = String.split (function '=' -> true | _ -> false) s 
    in try List.hd l, List.hd (List.tl l) with Failure _ -> raise (Failure ("can't parse"^s)))
let read_params str = parse_http_params str
let assoc key dic = try List.assoc key dic with Not_found -> raise (Failure (key ^ " not found"))

let fetch_request_token app = 
  fetch_request_token 
    ~host:host
    ~path:"/oauth/request_token"
    ~oauth_consumer_key:    app.Consumer.key
    ~oauth_consumer_secret: app.Consumer.secret 
    ()
  >>| fun res ->
  let res = read_params res in
  let token, secret = assoc "oauth_token" res, assoc "oauth_token_secret" res in
  "http://twitter.com/oauth/authorize?oauth_token="^token,
  { Token.token; secret }

let fetch_access_token 
    app ({Token.token= req_token; secret= req_secret}, verif) = 
  fetch_access_token 
    ~host:host
    ~path:"/oauth/access_token"
    ~oauth_consumer_key: app.Consumer.key
    ~oauth_consumer_secret: app.Consumer.secret
    ~oauth_token:req_token
    ~oauth_token_secret:req_secret
    ~verif:verif
    ()
  >>| fun res ->
  let res = read_params res in
  let acc_token, acc_secret, user = 
    assoc "oauth_token" res, 
    assoc "oauth_token_secret" res,
    assoc "screen_name" res
  in user, { Token.token= acc_token; secret= acc_secret }
      
let access ?proto ?(oauth_other_params=[]) ?(non_oauth_params=[]) meth host ?port path o = 
  Oauth.access
    ?proto
    ~host
    ?port
    ~path
    ~meth: (match meth with 
            | `GET -> `GET non_oauth_params
            | `POST -> `POST non_oauth_params)
    ~oauth_other_params
    o


