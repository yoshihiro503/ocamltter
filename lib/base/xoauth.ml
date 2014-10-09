(* High level oauth functions *)

open Spotlib.Spot
open Spotlib.Result.Open (* Monads are Result *)
open Util
open Http
open Oauth
open Ocaml_conv

let error e =
  prerr_endline & Http.string_of_error e;
  assert false

module Consumer = struct
  type t = { key : string; secret : string } with conv(ocaml)
  let dummy = { key = "Base64EncodedDataHereX";
                secret = "AnotherBase64EncodedDataHereAnotherBase64E"; }
end

(* Request and Access tokens are the same format but they should be
   strictly distinguished by their types. *)

module Request_token = struct
  type t = { token : string; secret : string } with conv(ocaml)
end

module Access_token = struct
  type t = { token : string; secret : string } with conv(ocaml)
end

let oauth app acc_tkn = {
  Oauth.consumer_key  = app.Consumer.key;
  consumer_secret     = app.Consumer.secret;
  access_token        = acc_tkn.Access_token.token;
  access_token_secret = acc_tkn.Access_token.secret;
}

let parse_http_params s = 
  String.split (function '&' -> true | _ -> false) s 
  |> List.map (fun s -> 
    match String.split1 (function '=' -> true | _ -> false) s with
    | None -> raise (Failure ("can't parse"^s))
    | Some (k,v) -> k,v)

let read_params str = parse_http_params str

let assoc key dic = try List.assoc key dic with Not_found -> raise (Failure (key ^ " not found"))

module type S = sig

  val oauth_signature_method : Oauth.signature_method

  val oauth_callback : string option option
    (** None : no oauth_callback header
        Some None : oauth_callback=oob  i.e.  Out Of Bound
        Some (Some url)
    *)

  val host : string

  val request_path : string

  val access_path : string

  val authorize_url : string
  (** "https://www.flickr.com/services/oauth/authorize?oauth_token=" *)

  val app : Consumer.t

end

module Make(A : S) = struct

  open A

  let fetch_request_token () =
    let oauth_callback_params = match oauth_callback with
      | None -> []
      | Some None -> ["oauth_callback", "oob"]
      | Some (Some url) -> ["oauth_callback", url]
    in
    fetch_request_token 
      ~oauth_signature_method
      ~host
      ~path:request_path
      ~oauth_other_params: oauth_callback_params
      ~oauth_consumer_key:    app.Consumer.key
      ~oauth_consumer_secret: app.Consumer.secret 
      ()
    >>| fun res ->
    let res = read_params res in
    { Request_token.token = assoc "oauth_token" res; 
      secret = assoc "oauth_token_secret" res 
    }
  
  let fetch_access_token ~req_token ~verif = 
    fetch_access_token 
      ~oauth_signature_method
      ~http_method:GET
      ~host
      ~path: access_path
      ~oauth_consumer_key:    app.Consumer.key
      ~oauth_consumer_secret: app.Consumer.secret
      ~oauth_token:        req_token.Request_token.token
      ~oauth_token_secret: req_token.Request_token.secret
      ~verif
      ()
    >>| fun res ->
    let res = read_params res in
    res, 
    { Access_token.token = assoc "oauth_token" res;
      secret = assoc "oauth_token_secret" res 
    }
        
  let authorize_cli_interactive () =
    match fetch_request_token () with
    | `Error e -> error e
    | `Ok req_token ->
        Printf.printf "Please grant access at %s%s\n" authorize_url req_token.Request_token.token;
        print_string "Give me the PIN: "; flush stdout;
        let verif = read_line () in
        match fetch_access_token ~req_token ~verif with
        | `Error e -> error e
        | `Ok (res, acc_token) -> res, acc_token
  
end

let access = Oauth.access
  
