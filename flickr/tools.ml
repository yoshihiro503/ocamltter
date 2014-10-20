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

