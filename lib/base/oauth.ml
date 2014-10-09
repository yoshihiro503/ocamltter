open Ocaml_conv
open Spotlib.Spot
open Util
open Http

let opt_param name param =
  match param with
    | None -> []
    | Some p -> [name, p]

let rng = Cryptokit.Random.device_rng "/dev/random"

let rfc3986_encode s = Http.url_encode s(*Netencoding.Url.encode s*)

type signature_method = 
  [ `Hmac_sha1
  | `Plaintext
  | `Rsa_sha1 of Cryptokit.RSA.key 
  ]

let string_of_signature_method : signature_method -> string = function
  | `Plaintext  -> "PLAINTEXT"
  | `Hmac_sha1  -> "HMAC-SHA1"
  | `Rsa_sha1 _ -> "RSA-SHA1"

let normalize_url url =
(*  let url = Neturl.parse_url ~enable_fragment:true url in
  let url = Neturl.remove_from_url ~query:true ~fragment:true url in
  Neturl.string_of_url url*)
  url

let string_of_timestamp t =
  let s = string_of_float t in
  String.sub s 0 (String.length s - 1)

let make_timestamp = Unix.time

let make_nonce () =
  Cryptokit.Random.string rng 16 
  |> Cryptokit.transform_string (Cryptokit.Hexa.encode ())

let hmac_sha1_hash text key =
  text 
  |> Cryptokit.hash_string (Cryptokit.MAC.hmac_sha1 key) 
  |> Base64.encode

let sha1_digest_info h =
  "\x30\x21\x30\x09\x06\x05\x2b\x0e\x03\x02\x1a\x05\x00\x04\x14" ^ h

let pkcs1_pad rsa_key v =
  let tLen = String.length v in
  let emLen = rsa_key.Cryptokit.RSA.size / 8 in
  "\x00\x01" ^ String.make (emLen - tLen - 3) '\xff' ^ "\x00" ^ v

(** Returns unsigned raw string *)
let rsa_sha1_hash' text rsa_key =
  text 
  |> Cryptokit.hash_string (Cryptokit.Hash.sha1 ()) 
  |> sha1_digest_info 
  |> pkcs1_pad rsa_key 

let rsa_sha1_hash text rsa_key =
  rsa_sha1_hash' text rsa_key
  |> Cryptokit.RSA.sign rsa_key 
  |> Base64.encode

let check_rsa_sha1_hash text rsa_key signature =
  try
    (text 
     |> Cryptokit.hash_string (Cryptokit.Hash.sha1 ()) 
     |> sha1_digest_info 
     |> pkcs1_pad rsa_key) 
    = (signature 
       |> Base64.decode 
       |> Cryptokit.RSA.unwrap_signature rsa_key)
  with _ -> false


let with_oauth_headers  
    ~oauth_signature_method
    ~oauth_consumer_key 
    ~oauth_timestamp 
    ~oauth_nonce 
    ~oauth_version
    ?oauth_token 
    ?oauth_signature
    ?(params = [])
    ~k () =
  k 
  & [ "oauth_signature_method" , string_of_signature_method oauth_signature_method;
      "oauth_consumer_key"     , oauth_consumer_key;
      "oauth_timestamp"        , string_of_timestamp oauth_timestamp;
      "oauth_nonce"            , oauth_nonce;
      "oauth_version"          , oauth_version;
    ] 
    @ opt_param "oauth_token"     oauth_token 
    @ opt_param "oauth_signature" oauth_signature
    @ params

let signature_base_string ~http_method ~url =
  with_oauth_headers ?oauth_signature:None ~k:(fun params ->
    let params = 
      (* make sure "oauth_signature" is not in the params *)       
      List.filter (function 
        | ("oauth_signature", _) -> false
        | _ -> true) params
    in
    List.map 
      rfc3986_encode
      [ Http.string_of_meth http_method
      ; normalize_url url
      ; 
        params 
        |> List.map (fun (k, v) -> rfc3986_encode k, rfc3986_encode v) 
        |> List.sort (fun (k,v) (k',v') ->
            match String.compare k k' with
            | 0 -> String.compare v v'
            | c -> c) 
        |> List.map (fun (k,v) -> k ^ "=" ^ v) 
        |> String.concat "&"
     
      ] 
    |> String.concat "&")


let pre_sign
    ~http_method ~url
    ~oauth_signature_method
    ~oauth_consumer_key ~oauth_consumer_secret
    ?oauth_token ?oauth_token_secret
    ~oauth_timestamp ~oauth_nonce ~oauth_version
    ?params
    ~k () =

  let key =
    rfc3986_encode oauth_consumer_secret ^ "&" ^
      match oauth_token_secret with
      | None -> ""
      | Some s -> rfc3986_encode s
  in

  let signature_base_string =
    signature_base_string
      ~http_method ~url
      ~oauth_signature_method
      ~oauth_consumer_key 
      ?oauth_token
      ~oauth_timestamp ~oauth_nonce ~oauth_version
      ?params
       (* ?oauth_token_secret ~oauth_consumer_secret *)
      () 
  in

  k oauth_signature_method signature_base_string key 

let sign = pre_sign ~k:(fun oauth_signature_method signature_base_string key ->
  match oauth_signature_method with
  | `Plaintext        -> rfc3986_encode key
  | `Hmac_sha1        -> hmac_sha1_hash signature_base_string key
  | `Rsa_sha1 rsa_key -> rsa_sha1_hash signature_base_string rsa_key)

let _check_signature ~oauth_signature = pre_sign ~k:(fun oauth_signature_method signature_base_string key ->
  match oauth_signature_method with
  | `Plaintext -> rfc3986_encode key = oauth_signature
  | `Hmac_sha1 -> hmac_sha1_hash signature_base_string key = oauth_signature
  | `Rsa_sha1 rsa_key -> check_rsa_sha1_hash signature_base_string rsa_key oauth_signature)

(** Auth params => a header line *)
let encode_authorization_params params =
  "Authorization",
  (params
      |> List.map (fun (k, v) ->
	k ^ "=\"" ^ String.escaped (rfc3986_encode v) ^ "\"")
      |> String.concat ",")

let authorization_header =
  with_oauth_headers ~k:(fun params -> 
    encode_authorization_params & ("OAuth realm", "") :: params)

let create_oauth_header 
    ~http_method ~url
    ~oauth_version 
    ~oauth_signature_method
    ~oauth_timestamp 
    ~oauth_nonce
    ~oauth_consumer_key 
    ~oauth_consumer_secret
    ?oauth_token 
    ?oauth_token_secret 
    params
    =
  let oauth_signature =
    sign
      ~http_method ~url
      ~oauth_version ~oauth_signature_method
      ~oauth_timestamp ~oauth_nonce
      ~oauth_consumer_key 
      ~params

      ~oauth_consumer_secret
      ?oauth_token ?oauth_token_secret
      () 
  in
  authorization_header
    ~oauth_version ~oauth_signature_method 
    ~oauth_timestamp ~oauth_nonce
    ~oauth_consumer_key
    ~params
    ?oauth_token
    ~oauth_signature 
    ()

let string_of_protocol = function
  | `HTTP -> "http"
  | `HTTPS -> "https"

(* CR jfuruse: 
   The distinction of oauth_other_params and non_oauth_params is not meaningful. *)
let gen_access
    ?handle_tweak
    ~protocol
    ~http_method ~host ?port ~path
    ?(oauth_version = "1.0") 
    ?(oauth_signature_method = `Hmac_sha1)
    ?(oauth_timestamp = make_timestamp ()) 
    ?(oauth_nonce = make_nonce ())
    ?oauth_token 
    ?oauth_token_secret
    ?(oauth_other_params=[])
    ?(non_oauth_params=[])
    ~oauth_consumer_key 
    ~oauth_consumer_secret 
    ()
    =
  let url = string_of_protocol protocol ^ "://" ^ host ^ path in
  let header = 
    create_oauth_header
      ~http_method ~url
      ~oauth_version
      ~oauth_signature_method
      ~oauth_timestamp
      ~oauth_nonce
      ~oauth_consumer_key 
      ~oauth_consumer_secret
      ?oauth_token
      ?oauth_token_secret
      (oauth_other_params @ non_oauth_params)
  in
  Http.by_curl 
    ?handle_tweak
    http_method protocol host ?port path ~headers:[header] ~params:non_oauth_params

let fetch_request_token ?(http_method=POST) = 
  gen_access 
    ~protocol: `HTTPS 
    ~http_method 
    ?oauth_token:None 
    ?oauth_token_secret:None 
    ~non_oauth_params:[]
 
let fetch_access_token ~verif ~oauth_token ~oauth_token_secret ?(http_method=POST) ?(oauth_other_params=[]) =
  gen_access 
    ~protocol: `HTTPS 
    ~http_method 
    ~oauth_token 
    ~oauth_token_secret 
    ~oauth_other_params:(("oauth_verifier",verif) :: oauth_other_params)
    ~non_oauth_params:[]

let access_resource ~oauth_token ~oauth_token_secret ?(http_method=GET) =
  gen_access ~http_method ~oauth_token ~oauth_token_secret

type t = {
  consumer_key:string; 
  consumer_secret:string;
  access_token:string; 
  access_token_secret:string;
} with conv(ocaml)

let access proto oauth meth host path params =
  access_resource ~protocol:proto ~http_method:meth ~host:host ~path:path
    ~oauth_consumer_key:oauth.consumer_key
    ~oauth_consumer_secret:oauth.consumer_secret
    ~oauth_token:oauth.access_token
    ~oauth_token_secret:oauth.access_token_secret
    ~non_oauth_params:params
    ()
