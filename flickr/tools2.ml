open Spotlib.Spot
open Api2

let json_format = !!% "%a@." Tiny_json.Json.format
let ocaml_format_with f = !!% "%a@." (Ocaml.format_with ~no_poly:true f)

let get_current_user o =
  let open Job in do_;
  x <-- Test.login o;
  (* CR jfuruse: we should have a nice embedding of content... *)      
  return (object method id = x#id method username = x#username end)

(* By extension. Maybe incorrect *)
let uploadable_by_name s =
  let _, ext = Filename.split_extension s in
  match String.lowercase ext with
  | ".jpg" | ".jpeg" | ".gif" | ".png" | ".tif" | ".tiff" -> true
  | ".wmv" | ".mp4" | ".avi" | ".mpg" | ".mpeg" | ".mov" -> true
  | _ -> false

(* Remove the duped photos in each photoset.
   Here "duped" means photos with the same non-empty title.
   Note that there is no real comparison of photos/videos.
*)
let delete_dups_in_sets o =
  let open Job in do_;
  (_info, psets) <-- Photosets.getList' o;
  flip mapM_ psets & fun set -> do_;
    let set_id = set#id in
    let set_title = set#title in
    [%p? (_, photos)] <-- Photosets.getPhotos' set_id o;
    let photos = List.map (fun x -> (x#title, x#id)) photos in
    let tbl = Hashtbl.create 107 in
    (); List.iter (fun (title, id) ->
      Hashtbl.alter tbl title (function
        | None -> Some [id]
        | Some ids -> Some (id :: ids))) photos;
    flip mapM_ (Hashtbl.to_list tbl) & fun (title, ids) ->
      match ids with
      | [] -> assert false
      | [_] -> return ()
      | xs -> 
          !!% "Dups: set: %s (%s) title: %s ids: %s@."
            set_title
            set_id title (String.concat "," xs);
          if title <> "" then 
            flip mapM_ (List.tl xs) & fun photo_id ->
              !!% "Deleting %s : %s@." title photo_id;
              Photos.delete photo_id o >>| fun () -> !!% "Deleted@."
          else return ()

(* 

   This is an upload algorithm which suits the author's personal situation. 
   This may not fit with your purpose at all! Be careful. Read the source!

   * Currently this is just for uploading. No real syncing is provided.

   * In one photoset, media(photos/videos) are identified by their title.

   * Titles of media are their basenames without file extension:
     "foo" for "directory/foo.jpg" and "bar" for "dir/bar.mpg".

   * Therefore, you CANNOT have files like "family.jpg" and "family.mpg"
     in one photoset. If one of them is uploaded, then the other is simply
     skipped and never uploaded. 
     There is no safety check of this (for the moment).

   * Currently there is no syncing. Even if your local media are
     modified, the updated versions are never uploaded until the corresponding
     files are removed from the photoset by some other means.

   Algorithm:

   * A medium is uploaded with the following informatin:

     * Title: its basename without extension
     * Tags: "uploading" and "photoset_<photoset>"

   * If uploading is successful, the medium is added to the photoset.

   * If the photoset addition is successful, the tag "uploading" is removed.

   * For one medium, the function tries to recover communication errors
     at most 2 times (i.e. 3 trials).

   Things to do

   * If a photoset addition fails, there remains a medium with "uploading"
     tag which is not in any photoset. It should be recovred by "getNotInSet"
     method and add into the appropriate photoset in later trials. 

   * If a medium is added to the photoset but if "uploading" tag removal
     fails, the medium remains in the photoset with the tag. It is not critical
     but the function should clean the tag in later trials.
*)
let uploads ~photoset img_files o =
  let open Job in do_;
  (_info, psets) <-- Photosets.getList' o;

  (); !!% "Got existing photosets (%d)@." & List.length psets;

  let psid_opt = 
    (* maybe overridden later *)
    ref (
      List.find_opt (fun pset -> pset#title = photoset) psets 
      |> Option.map (fun pset -> pset#id)
    )
  in

  photos <-- begin match !psid_opt with
    | None -> return []
    | Some psid -> do_;
        (); !!% "Getting photos of photoset %s (id=%s)@." photoset psid;
        [%p? (_, ps) ] <-- Photosets.getPhotos' psid o;
        return & List.map (fun p -> (p#title, p#id)) ps
    end;
  
  let photos = ref photos in

  let up ~title img_file = do_;
    (); !!% "Uploading %s@." img_file;
    photo_id <-- Upload.upload 
      ~title img_file 
      ~tags:["uploading"; "photoset_" ^ photoset] 
      o;
    
    (); photos := (title, photo_id) :: !photos;

    (* moving to the photoset *)
    begin match !psid_opt with
    | None -> do_;
        (); !!% "Creating new photoset %s with photo %s@." photoset img_file;
        res <-- Photosets.create ~title:photoset ~primary_photo_id: photo_id o;
        (); psid_opt := Some res#id;
        return ()
    | Some psid ->
        !!% "Adding %s to photoset %s@." img_file photoset;
        Photosets.addPhoto psid ~photo_id o
    end;
    
    (* remove "uploading" tag *)
    Photos.setTags photo_id ["photoset_" ^ photoset] o;
    return photo_id
  in

  flip mapM_ img_files (fun img_file ->
    let title = Filename.(basename *> split_extension *> fst) img_file in
    if List.mem_assoc title !photos then begin
      (* !!% "%s is already in the photoset@." img_file; *)
      return ()
    end else begin
      up ~title img_file >>| !!% "Uploaded as id = %s@."
    end);

  (* reorder *)
  match !psid_opt with
  | None -> return ()
  | Some psid -> do_;
      [%p? (_, ps) ] <-- Photosets.getPhotos' psid ~extras:["date_taken"] o;
      let ps' =
        let open List in
        sort (fun (_,t1) (_,t2) -> compare t1 t2)    
          & filter_map (fun p -> Option.do_;
              (* Flickr returns "datetaken" for "date_taken" :-< *)
              dt <-- assoc_opt "datetaken" p#extras;
              match dt with
              | Tiny_json.Json.String s -> return (p, s)
              | _ -> None) ps
      in
     (); flip List.iter ps' (fun (p,t) ->
       Format.eprintf "%s : %s : %s@." p#id p#title t);
     (); Format.eprintf "Reordering...@.";
     Photosets.reorderPhotos psid (List.map (fun (p,_) -> p#id) ps') o;
     (); Format.eprintf "Reordered@.";
     return ()

