open Spotlib.Spot
open Twitter

module Flickr_oauth_conf = struct
  let oauth_signature_method = `Hmac_sha1
  let oauth_callback = Some None (* oob *)
  let host = "www.flickr.com"
  let request_path = "/services/oauth/request_token"
  let access_path = "/services/oauth/access_token"
  let authorize_url = "https://www.flickr.com/services/oauth/authorize?oauth_token="
  let app = App.app
end

include Xoauth.Make(Flickr_oauth_conf)

let auth_file = "ocaml_flickr.auth"

let load_auth () =
  match Ocaml.load_with_exn Xoauth.Access_token.t_of_ocaml auth_file with
  | [a] -> a
  | _ -> assert false

let get_acc_token () =
  try load_auth () with
  | _ -> 
      let _res, acc_token = authorize_cli_interactive () in
      Ocaml.save_with Xoauth.Access_token.ocaml_of_t ~perm:0o600 auth_file [acc_token];
      acc_token

let get_oauth () =
  let acc_token = get_acc_token () in
  Xoauth.oauth Flickr_oauth_conf.app acc_token

let xml = "<?xml version=\"1.0\" encoding=\"utf-8\" ?>
<rsp stat=\"fail\">
	<err code=\"2\" msg=\"No photo specified\" />
</rsp>
"

let _ = Xml.parse_string xml

let o = get_oauth ()

let error = function
  | (`Http _ | `Curl _ as e) -> Xoauth.error e
  | `Json (e, s) -> 
      !!% "Json: %a@." Flickr.Json.format_error e;
      !!% "  %S@." s;
      assert false
  | `Json_conv e ->
      !!% "Json_conv: %a@." Json_conv.format_full_error e;
      assert false
  | `API fail ->
      !!% "API: %a@." Flickr.Fail.format fail;
      assert false
  | `Load (name, exn) ->
      !!% "Local file load failure: %s: %a@." name Exn.format exn;
      assert false
  | `Xml_parse (s, exn) ->
      !!% "XML parse failure: %a : %s@." Exn.format exn s;
      assert false

let () =
  match
    Xoauth.access `HTTPS o
      `GET
      "api.flickr.com"
      "/services/rest"
      ~oauth_other_params: [ "nojosoncallback", "1"
                           ; "format", "json"
                           ; "method", "flickr.test.login"
                           ]
  with
  | `Error e -> error e
  | `Ok s -> prerr_endline s

let () =
  match Flickr.Photosets.getList o with
  | `Error e -> error e
  | `Ok v -> !!% "%a@." (Ocaml.format_with Flickr.Photosets.GetList.ocaml_of_photoset) v

let () =
  match Flickr.Photosets.getPhotos "72157648394310161" o with
  | `Error e -> error e
  | `Ok v -> !!% "%a@." (Ocaml.format_with Flickr.Photosets.GetPhotos.ocaml_of_photoset) v

let () =
  match Flickr.Photosets.removePhotos "72157648394310161" ["700261373"] o with
  | `Error e -> error e
  | `Ok () -> !!% "done@."

let () =
  match Flickr.People.getUploadStatus o with
  | `Error e -> error e
  | `Ok v -> !!% "%a@." (Ocaml.format_with Flickr.People.GetUploadStatus.ocaml_of_t) v

(*
  | `Ok j -> !!% "%a@." Tiny_json.Json.format j
*)

let () =
  match Flickr.Upload.upload ~is_family:true "test.jpg" o with
  | `Error e -> error e
  | `Ok v -> !!% "%s@." (Xml.show v)
      
