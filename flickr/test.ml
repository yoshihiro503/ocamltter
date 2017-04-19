open Spotlib.Spot
open Api
open Tools
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
  
(*
let () =
  let pid = Upload.upload ~tags:["tags"] "test.jpg" o |> fail_at_error in
  Tags.getListPhoto pid o |> fail_at_error |> ocaml_format_with Tags.GetListPhoto.ocaml_of_t

let _ = exit 0
*)

let getInfo pid o =
  match Photos.getInfo pid o with
  | Error e -> error e
  | Ok j -> ocaml_format_with Photos.GetInfo.ocaml_of_photo j


(*
let () = delete_dups_in_sets ()
*)

(*
let () = Photosets.getList o |> fail_at_error |> ocaml_format_with Photosets.GetList.ocaml_of_t
*)

(*
let () = Flickr.Upload.upload "test.jpg" o |> fail_at_error |> 
    fun xs ->    prerr_endline (String.concat ", " xs)
*)

let user = get_current_user o |> fail_at_error

let () = !!% "id=%s username=%s@." user#id user#username

let photos = Photos.search ~user_id:user#id o |> fail_at_error

let () = ocaml_format_with Photos.Search.ocaml_of_photos photos

let () =
  match photos#photo with
  | [] -> assert false
  | p::_ ->
      Photos.getInfo p#id o |> fail_at_error 
      |> fun p -> prerr_endline p#dates#taken
