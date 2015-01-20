open Spotlib.Spot
open Json_conv
open Result

module Oauth = Api.Oauth

module Auth = struct
  module Oauth = struct
    let checkToken (* _oauth_token *) o = Job.create & fun () -> Api.Auth.Oauth.checkToken o
  end
end

module Error = Api.Error
  
module Page = struct

  (** Page to stream interface *)

  let to_seq
      : (per_page: int -> page: int -> ('res, 'error) Job.t)
      -> per_page:int
      -> ('res -> 'info)
      -> ('res -> 'a list)
      -> (('info * ('a, 'error) Job.Seq.t), 'error) Job.t
    = fun f ~per_page get_info get_list ->
    let open Job in
    let info = ref None in
    let rec loop page : ('a list, 'error) Job.Seq.t = do_;
      res <-- f ~per_page ~page;
      (); begin match !info with
          | Some _ -> ()
          | None -> info := Some (get_info res)
          end;
      (); assert (res#perpage = per_page);
      let final = res#perpage * res#page >= res#total in
      let next =
        if final then Job.return `None
        else loop (page+1)
      in
      return & `Some (get_list res, next)
    in
    do_
    ; seq <-- loop 1
    ; match !info with
      | None -> assert false
      | Some info -> return (info, Seq.flatten & return seq)

  let to_list x = Job.do_
    ; (info, seq) <-- x
    ; xs <-- Job.of_seq seq;
    return (info, xs)

end

module TagList = struct
  type t = string list [@@deriving conv{ocaml}]

  let json_of_t xs = json_of_string (String.concat " " xs)
  let t_of_json ?trace j =
    string_of_json ?trace j >>= fun s -> 
    return & String.split (function ' ' -> true | _ -> false) s
  let t_of_json_exn = Json_conv.exn t_of_json
end

(* ************************* APIs ***********************)
  
module Photos = struct  

(*
  let int_of_privacy_filter = function
    | `Public -> 1
    | `Friends -> 2
    | `Family -> 3
    | `Friends_and_family -> 4
    | `Private -> 5

  module Media = struct
    type t = [`All     as "all"
             | `Photos as "photos"
             | `Videos as "videos" ] 
    [@@deriving conv{ocaml; json}]
  end
*)

  let raw_getNotInSet ~per_page ~page (* ?privacy_filter ?media *) 
      ?tags o = 
    Job.create & fun () -> Api.Photos.raw_getNotInSet ~per_page ~page ?tags o

  let getNotInSet ?(per_page=100) ?tags o =
    Page.to_seq (raw_getNotInSet ?tags o) ~per_page
      (fun res -> res#total)
      (fun res -> res#photo)

  let getNotInSet' ?per_page ?tags o =
    getNotInSet ?per_page ?tags o |> Page.to_list

  let getInfo photo_id o = Job.create & fun () -> Api.Photos.getInfo photo_id o
    
  let getExif photo_id o = Job.create & fun () -> Api.Photos.getExif photo_id o

  let addTags photo_id tags o = Job.create & fun () -> Api.Photos.addTags photo_id tags o

  let setTags photo_id tags o = Job.create & fun () -> Api.Photos.setTags photo_id tags o

  let delete photo_id o = Job.create & fun () -> Api.Photos.delete photo_id o

(*
{ "photos": { "page": 1, "pages": 34, "perpage": 100, "total": "3343",
              "photo": [ { "id": "15379632298", "owner": "9582328@N04",
                           "secret": "c90f3ac8c1", "server": "3939",
                           "farm": 4, "title": "test", "ispublic": 0,
                           "isfriend": 0, "isfamily": 0 },
                         { "id": "15505863036", "owner": "9582328@N04",
*)

  let search ?user_id ?tags ?text ?per_page ?page o =
    Job.create & fun () -> Api.Photos.search ?user_id ?tags ?text ?per_page ?page o

end

module Photosets = struct

  let create ~title ~primary_photo_id o = Job.create & fun () ->
    Api.Photosets.create ~title ~primary_photo_id o

  let raw_getList ~per_page ~page o = 
    Job.create & fun () -> Api.Photosets.raw_getList ~per_page ~page o

  let getList ?(per_page=500) o =
    Page.to_seq (raw_getList o) ~per_page
      (fun psets -> object
        method cancreate = psets#cancreate
        method total = psets#total
      end)
      (fun psets -> psets#photoset)

  let getList' ?per_page o =
    getList ?per_page o |> Page.to_list
        
  let raw_getPhotos photoset_id ~per_page ~page o =
    Job.create & fun () -> Api.Photosets.raw_getPhotos photoset_id ~per_page ~page o

  (* CRv2 jfuruse: todo: Fancy lazy loading *)
  let getPhotos ?(per_page=500) photoset_id o =
    Page.to_seq (raw_getPhotos photoset_id o) ~per_page
      (fun pset -> object
        method id        = pset#id
        method primary   = pset#primary
        method owner     = pset#owner
        method ownername = pset#ownername
        method total     = pset#total
        method title     = pset#title
      end)
      (fun pset -> pset#photo)

  let getPhotos' ?per_page photoset_id o =
    getPhotos ?per_page photoset_id o |> Page.to_list

  let removePhotos photoset_id photo_ids o =
    Job.create & fun () -> Api.Photosets.removePhotos photoset_id photo_ids o

  let addPhoto photoset_id ~photo_id o =
    Job.create & fun () -> Api.Photosets.addPhoto photoset_id ~photo_id o

end

module People = struct

  let getUploadStatus o = Job.create & fun () -> Api.People.getUploadStatus o

end

module Tags = struct
    (* flickr.tags.getListPhoto *)

  let getListPhoto photo_id o = Job.create & fun () -> Api.Tags.getListPhoto photo_id o

end

module Test = struct

  let login o =
    Job.create & fun () -> Api.Test.login o

end

module Upload = struct

  let upload 
      ?is_public
      ?is_friend
      ?is_family
      ?hidden
      ?title
      ?description
      ?tags (** Must not contain white space chars, but not check performed yet *) 
      img_file o =
    Job.create & fun () ->
      Api.Upload.upload 
        ?is_public
        ?is_friend
        ?is_family
        ?hidden
        ?title
        ?description
        ?tags
        img_file o
    
(*
safety_level (optional)
Set to 1 for Safe, 2 for Moderate, or 3 for Restricted.
content_type (optional)
Set to 1 for Photo, 2 for Screenshot, or 3 for Other.
*)
    
end

let run_and_fail_at_error t = match Job.run t with
  | `Ok v -> v
  | `Error (e, _) -> Api.Error.fail e

