open Spotlib.Spot
open Api2
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
        if p#kind = `Ok Unix.S_REG then acc +::= p#path);
      !acc
    in
    let photos = 
      flip List.filter photos & fun s ->
        let b = Tools2.uploadable_by_name s in
        if not b then !!% "WARNING: %s is not uploadable I think. Therefore ignored@." s;
        b
    in
    (* not try to make an empty photoset *)
    if photos <> [] then begin
      match 
        Job.run & Job.retry (fun conseq_fails e ->
          match e with
          | `Http ( (502 | 504 as n), _ ) ->
              !!% "Server side error (%d): %a@." n Api2.format_error e;
              !!% "Failed. Wait 1 min then retry...@.";
              Unix.sleep 60;
              `Ok conseq_fails
          | _ ->
            if conseq_fails >= 3 then begin
              !!% "Failed 3 times! Check your setting!@.";
              (* CR jfuruse: we have no good way to reset conseq_fails *)
              `Error e
            end else begin
              !!% "Error: %a@." Api2.format_error e;
              !!% "Failed. Wait 1 min then retry...@.";
              Unix.sleep 60;
              `Ok (conseq_fails + 1)
            end) 0 
        & Tools2.uploads ~photoset photos o 
      with
      | `Ok () -> ()
      | `Error (desc, _) -> Api2.error desc
    end
