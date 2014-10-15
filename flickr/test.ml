open Spotlib.Spot
open Flickr

(*
let xml = "<?xml version=\"1.0\" encoding=\"utf-8\" ?>
<rsp stat=\"fail\">
	<err code=\"2\" msg=\"No photo specified\" />
</rsp>
"

let _ = Xml.parse_string xml
*)

let auth_file = "ocaml_flickr.auth"

let o = get_oauth auth_file

let json_format = !!% "%a@." Tiny_json.Json.format
let ocaml_format_with f = !!% "%a@." (Ocaml.format_with f)

let fail_at_error = function
  | `Ok v -> v
  | `Error e -> error e

let find_dups_in_sets () =
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
    flip Hashtbl.iter tbl (fun title ids ->
      match ids with
      | [] -> assert false
      | [_] -> ()
      | xs -> 
          Format.eprintf "Dups: set: %s (%s) title: %s ids: %s@."
            set_title
            set_id title (String.concat "," xs))

let getInfo pid o =
  match Photos.getInfo pid o with
  | `Error e -> error e
  | `Ok j -> ocaml_format_with Photos.GetInfo.ocaml_of_resp j

let delete_dups_in_sets () =
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
            

let () = delete_dups_in_sets ()
