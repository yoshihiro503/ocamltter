open Spotlib.Spot
open Flickr

let json_format = !!% "%a@." Tiny_json.Json.format
let ocaml_format_with f = !!% "%a@." (Ocaml.format_with ~no_poly:true f)

let fail_at_error = function
  | `Ok v -> v
  | `Error e -> error e

let get_current_user o =
  let open Result in
  Flickr.Test.login o >>= fun x -> 
  (* CR jfuruse: we should have a nice embedding of content... *)      
  return (object method id = x#id method username = x#username#content end)

(* Remove the duped photos in each photoset.
   Here "duped" means photos with the same non-empty title.
   Note that there is no real comparison of photos/videos.
*)
let delete_dups_in_sets o =
  let psets = Photosets.getList o |> fail_at_error in
  flip List.iter psets#photoset & fun set ->
    let set_id = set#id in
    let set_title = set#title#content in
    let photos = Photosets.getPhotos set_id o |> fail_at_error 
                 |> fun x -> x#photo 
                 |> List.map (fun x -> (x#title, x#id))
    in
    let tbl = Hashtbl.create 107 in
    List.iter (fun (title, id) ->
      Hashtbl.alter tbl title (function
        | None -> Some [id]
        | Some ids -> Some (id :: ids))) photos;
    flip Hashtbl.iter tbl & fun title ids ->
      match ids with
      | [] -> assert false
      | [_] -> ()
      | xs -> 
          !!% "Dups: set: %s (%s) title: %s ids: %s@."
            set_title
            set_id title (String.concat "," xs);
          if title <> "" then 
            flip List.iter (List.tl xs) & fun photo_id ->
              !!% "Deleting %s : %s@." title photo_id;
              match Photos.delete photo_id o with
              | `Error e -> error e
              | `Ok () -> !!% "Deleted@."

let uploads ~photoset img_files o =
  let psets = (Photosets.getList o |> fail_at_error)#photoset in
  !!% "Got existing photosets (%d)@." & List.length psets;
  let psid_opt = 
    (* maybe overridden later *)
    ref (
      List.find_opt (fun pset -> pset#title#content = photoset) psets 
      |> Option.map (fun pset -> pset#id)
    )
  in
  let photos =
    ref (
      match !psid_opt with
      | None -> []
      | Some psid ->
          !!% "Getting photos of photoset %s (%s)@." photoset psid;
          let ps = (Photosets.getPhotos psid o |> fail_at_error)#photo in
          List.map (fun p -> (p#title, p#id)) ps
    )
  in

  let up ~title img_file =
    let rec upload () =
      !!% "Uploading %s@." img_file;
      match 
        Upload.upload 
          ~title img_file 
          ~tags:["uploading"; "photoset_" ^ photoset] 
          o
      with
      | `Error e -> `Error (e, upload)
      | `Ok photo_id -> 
          photos := (title, photo_id) :: !photos;
          add_to_photoset photo_id

    and add_to_photoset photo_id =
      (* moving to the photoset *)
      match !psid_opt with
      | None ->
          !!% "Creating new photoset %s with photo %s@." photoset img_file;
          begin match 
            Photosets.create ~title:photoset ~primary_photo_id: photo_id o
          with
          | `Error e -> `Error (e, fun () -> add_to_photoset photo_id)
          | `Ok res -> 
              psid_opt := Some res#id;
              clean_uploading_tag photo_id
          end
      | Some psid ->
          !!% "Adding %s to photoset %s@." img_file photoset;
          begin match 
              Photosets.addPhoto psid ~photo_id o 
          with
          | `Error e -> `Error (e, fun () -> add_to_photoset photo_id)
          | `Ok () -> 
              clean_uploading_tag photo_id
          end              

    and clean_uploading_tag photo_id =
      (* remove "uploading" tag *)
      match
        Photos.setTags photo_id ["photoset_" ^ photoset] o 
      with
      | `Error e -> `Error (e, fun () -> clean_uploading_tag photo_id)
      | `Ok () -> `Ok photo_id
    in

    let trial = 2 in
    let wait = 30 in

    let rec try_ left f =
      match f () with
      | `Error (e, _) when left = 0 -> 
          format_error Format.stderr e;
          !!% "No more retry@.";
          `Error e
      | `Error (e, retry) ->
          format_error Format.stderr e;
          !!% "Retrying (left=%d) after %d secs...@." left wait;
          Unix.sleep wait;
          try_ (left-1) retry
      | (`Ok _ as ok) -> ok
    in
    try_ trial upload
  in

  flip List.iter img_files & fun img_file ->
    let title = Filename.(basename *> split_extension *> fst) img_file in
    if List.mem_assoc title !photos then
      !!% "%s is already in the photoset@." img_file
    else 
      match up ~title img_file with
      | `Ok pid -> 
          !!% "Uploaded as id = %s@." pid
      | `Error _e ->
          !!% "Errors reached critical level. Aborting.@.";
          exit 1
