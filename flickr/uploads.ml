open Spotlib.Spot
open Api
module Xml = OCamltter_oauth.Xml

let auth_file = "ocaml_flickr.auth"

let o = Oauth.get_oauth auth_file

let rev_dirs = ref []

let () = Arg.parse [] (fun x -> rev_dirs := x :: !rev_dirs) "uploads dirs.."
let dirs = List.rev !rev_dirs

let () = 
  let dirs = flip List.filter dirs & fun dir ->
    let b = File.Test._d dir in
    if not b then !!% "WARNING: %s is not a directory. Skipped.@." dir;
    b
  in
  flip List.iter dirs & fun dir ->
    let photoset = Filename.basename dir in
    assert (photoset <> "");
    let photos = 
      let acc = ref [] in
      Unix.Find.find ~follow_symlink:true [dir] ~f:(fun p ->
        if p#kind = Ok Unix.S_REG then acc +::= p#path);
      !acc
    in
    let photos = 
      flip List.filter photos & fun s ->
        let b = Tools.uploadable_by_name s in
        if not b then !!% "WARNING: %s is not uploadable I think. Therefore ignored@." s;
        b
    in
    (* not try to make an empty photoset *)
    if photos <> [] then Tools.uploads ~photoset photos o
