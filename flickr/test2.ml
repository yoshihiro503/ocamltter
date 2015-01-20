open Spotlib.Spot
open Api2
open Tools2
module Xml = OCamltter_oauth.Xml

let xml_parse () =
  let xml = "<?xml version=\"1.0\" encoding=\"utf-8\" ?>
<rsp stat=\"fail\">
	<err code=\"2\" msg=\"No photo specified\" />
</rsp>
" in
  Xml.parse_string xml

let () = ignore & xml_parse (); prerr_endline "xml parse done"
  

let auth_file = "ocaml_flickr.auth"

let o = Oauth.get_oauth auth_file

let () = !!% "OAuth: %a@." (Ocaml.format_with Oauth.ocaml_of_t) o
  
let getInfo pid o =
  match Job.run & Photos.getInfo pid o with
  | `Error (e, _) -> Error.fail e
  | `Ok j -> ocaml_format_with Api.Photos.GetInfo.ocaml_of_photo j

let () =
  let open Job in
  run_and_fail_at_error & 
  get_current_user o >>= fun user ->
  !!% "id=%s username=%s@." user#id user#username;
  Photos.search ~user_id:user#id o >>= fun photos ->
  ocaml_format_with Api.Photos.Search.ocaml_of_photos photos;
  match photos#photo with
  | [] -> assert false
  | p::_ ->
      Photos.getInfo p#id o >>| fun p -> prerr_endline p#dates#taken
