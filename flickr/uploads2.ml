open Spotlib.Spot
open Api2
module Xml = OCamltter_oauth.Xml

let auth_file = "ocaml_flickr.auth"

let o = Oauth.get_oauth auth_file

let () =
  Format.eprintf "%a@."
    (Ocaml.format_with Api.People.GetUploadStatus.ocaml_of_user )
    (Result.from_Ok & Job.run & People.getUploadStatus o)

let rev_dirs = ref []

let remove_non_local = ref false

let upload_movie = ref false

let () = Arg.parse 
  [ "-remove-non-local", Arg.Set remove_non_local, "remove photos not in local directory from photoset (the photo still exists in the flickr)"
  ; "-upload-movie", Arg.Set upload_movie, "upload movies too"
  ] 
  (fun x -> rev_dirs := x :: !rev_dirs) "uploads dirs.."

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
        if p#base = "@eaDir" then Unix.Find.prune (); (* synology nas fix *)
        if p#kind = Ok Unix.S_REG then acc +::= p#path);
      !acc
    in
    let targets = 
      flip List.filter photos & fun s ->
        match Tools2.uploadable_by_name s with
        | Some `Image -> true
        | Some `Movie when !upload_movie -> true
        | Some `Movie ->  !!% "WARNING: %s is a movie and therefore skipped@." s; false
        | _ -> !!% "WARNING: %s is not uploadable I think. Therefore ignored@." s; false
    in
    (* not try to make an empty photoset *)
    if targets <> [] then begin (* CR jfuruse: impossible to remove locally removed photos with empty targets *)
      match 
        Job.run & Job.retry (fun conseq_fails e ->
          match e with
          | `Http ( (502 | 504 as n), _ ) ->
              !!% "Server side error (%d): %a@." n Api2.format_error e;
              !!% "Failed. Wait 1 min then retry...@.";
              Unix.sleep 60;
              Ok conseq_fails
          | _ ->
            if conseq_fails >= 3 then begin
              !!% "Failed 3 times! Check your setting!@.";
              (* CR jfuruse: we have no good way to reset conseq_fails *)
              Error e
            end else begin
              !!% "Error: %a@." Api2.format_error e;
              !!% "Failed. Wait 1 min then retry...@.";
              Unix.sleep 60;
              Ok (conseq_fails + 1)
            end) 0 
        & Tools2.uploads ~remove_non_local:!remove_non_local 
          ~photoset ~existing:photos targets o 
      with
      | Ok () -> ()
      | Error (desc, _) -> Api2.error desc
    end
