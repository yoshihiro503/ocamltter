open Spotlib.Spot
open Flickr
module Xml = Oauthlib.Xml

let auth_file = "ocaml_flickr.auth"

let o = get_oauth auth_file

let rev_files = ref []

let () = Arg.parse [] (fun x -> rev_files := x :: !rev_files) "uploads photoset photos.."

let () = match List.rev !rev_files with
  | photoset :: photos ->
      Tools.uploads ~photoset photos o
  | _ -> assert false
