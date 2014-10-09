open Spotlib.Spot
open Twitter

open Ocaml_conv
(*
open Meta_conv.Open
*)
open Json_conv

open Result

module Json = struct
  include Tiny_json.Json
  let json_of_t x = x
  let t_of_json ?trace:_ x = `Ok x

  let ocaml_of_t t = Ocaml.String (show t)
  let t_of_ocaml = Ocaml_conv.Helper.of_deconstr (function
    | Ocaml.String s -> parse s
    | _ -> failwith "Ocaml.String expected")

  let ocaml_of_exn = ocaml_of_string *< Exn.to_string

  type error = 
    | NotObject of t
    | InvalidField of string
    | CastErr of string
    | UnknownErr of string
    | NotJsonErr of exn
    | NoJsonResponse
  with conv(ocaml_of)

  let format_error = Ocaml.format_with ocaml_of_error

  let of_exn = function
    | JSON_NotObject t -> NotObject t
    | JSON_InvalidField s -> InvalidField s
    | JSON_CastErr s -> CastErr s
    | JSON_UnknownErr s -> UnknownErr s
    | exn -> NotJsonErr exn

  let parse s = try `Ok (parse s) with exn -> `Error (`Json (of_exn exn, s))

  type sint = int with conv(ocaml)

  let sint_of_json ?(trace=[]) = function
    | (Number _ as j) -> int_of_json ~trace j
    | (String s as t) -> 
        begin try `Ok (int_of_string s) with
        | e -> `Error (Meta_conv.Error.Exception e, t, `Node t :: trace)
        end
    | t -> `Error (Meta_conv.Error.Exception (Failure "Number or String expected"), t, `Node t :: trace)

  let json_of_sint = json_of_int

  type ibool = bool with conv(ocaml)

  let ibool_of_json ?trace j = 
    sint_of_json ?trace j >>= fun n -> `Ok (n <> 0)

  let json_of_ibool b = Number (if b then "1" else "0")
end

open Json

type 'a jsonFlickrApi = [ `jsonFlickrApi of 'a ] with conv(json, ocaml)

let raw_api ?(post=false) o m fields = 
  Xoauth.access `HTTPS o
    (if post then Http.POST else Http.GET)
    "api.flickr.com"
    "/services/rest"
    ~oauth_other_params: ([ "method", m
                          ; "format", "json" ]
                          @ fields)

let json_api ?post o m fields =
  raw_api ?post o m fields
  >>= fun s ->
      (* jsonFlickrApi(JSON) *)
      let len = String.length s in
      match 
        String.sub s 0 14,
        String.sub s 14 (len - 15),
        s.[len-1]
      with
      | "jsonFlickrApi(", s, ')' -> Json.parse s
      | exception _ -> `Error (`Json (Json.NoJsonResponse, s))
      | _ -> `Error (`Json (Json.NoJsonResponse, s))

let lift_error f s = match f s with
  | (`Ok _ as v) -> v
  | `Error e -> `Error (`Json_conv e)
    
module Fail = struct

  type t = { 
    stat : string (** "fail" *);
    code : sint;
    message : string 
  } with conv(json, ocaml)

  let format = Ocaml.format_with ocaml_of_t

  let check j =
    match lift_error t_of_json j with
    | `Ok e when e.stat = "fail" -> `Error (`API e)
    | _ -> `Ok j
end

type content = { _content : string } with conv(json, ocaml)
  
module Photosets = struct

  module GetList = struct

    type set = { id : string;
                 primary : string;
                 secret : string;
                 server : string;
                 farm : sint;
                 photos : Json.sint;
                 videos : Json.sint;
                 title : content;
                 description : content;
                 needs_interstitial : ibool;
                 visibility_can_see_set : ibool;
                 count_views : sint;
                 count_comments : sint;
                 can_comment : ibool;
                 date_create : string;
                 date_update : string
               }
  
    and photoset = { cancreate : ibool;
                     page : sint;
                     pages : sint;
                     perpage : sint;
                     total : sint;
                     photoset : set list }
  
    and t = { photosets : photoset;
              stat : string; }

    with conv(json, ocaml_of )

  end

  let raw_getList ?(page=1) o = 
    assert (page > 0);
    json_api o "flickr.photosets.getList"
      [ "api_key", App.app.Xoauth.Consumer.key
      ; "page", string_of_int page
      ]
    >>= Fail.check
    >>= lift_error GetList.t_of_json

(*
user_id (Optional)
The NSID of the user to get a photoset list for. If none is specified, the calling user is assumed.
per_page (Optional)
The number of sets to get per page. If paging is enabled, the maximum number of sets per page is 500.
primary_photo_extras (Optional)
A comma-delimited list of extra information to fetch for the primary photo. Currently supported fields are: license, date_upload, date_taken, owner_name, icon_server, original_format, last_update, geo, tags, machine_tags, o_dims, views, media, path_alias, url_sq, url_t, url_s, url_m, url_o
*)

  (* CRv2 jfuruse: todo: Fancy lazy loading *)
  let getList o =
    let open GetList in
    let rec f n st =
      raw_getList o ~page:n 
      >>= fun { photosets = psets } ->
        if psets.perpage * n > psets.total then
          (* last page *)
          `Ok { psets with photoset = st @ psets.photoset; }
        else f (n+1) (st @ psets.photoset)
    in
    f 1 []

  module Photo = struct
    type t = {
      id : string;
      secret : string;
      server : string;
      farm : sint;
      title : string;
      isprimary : ibool;
      ispublic : ibool;
      isfriend : ibool;
      isfamily : ibool 
    } with conv(json,ocaml)
  end

  module GetPhotos = struct

    type t = { photoset : photoset; stat : string; }
        
    and photoset = {
      id : string;
      primary : string;
      owner : string;
      ownername : string;
      photo : Photo.t list;
      page : sint;
      per_page : sint;
      perpage : sint;
      pages : sint;
      total : sint;
      title : string;
    }
    with conv(json,ocaml)

  end


  let raw_getPhotos photoset_id ?(page=1) o =
    assert (page > 0);
    json_api o "flickr.photosets.getPhotos"
      [ "api_key", App.app.Xoauth.Consumer.key
      ; "photoset_id", photoset_id
      ; "page", string_of_int page
      ]
    >>= Fail.check
    >>= lift_error GetPhotos.t_of_json

(*
extras (Optional)
A comma-delimited list of extra information to fetch for each returned record. Currently supported fields are: license, date_upload, date_taken, owner_name, icon_server, original_format, last_update, geo, tags, machine_tags, o_dims, views, media, path_alias, url_sq, url_t, url_s, url_m, url_o
privacy_filter (Optional)
Return photos only matching a certain privacy level. This only applies when making an authenticated call to view a photoset you own. Valid values are:
1 public photos
2 private photos visible to friends
3 private photos visible to family
4 private photos visible to friends & family
5 completely private photos
per_page (Optional)
Number of photos to return per page. If this argument is omitted, it defaults to 500. The maximum allowed value is 500.
media (Optional)
Filter results by media type. Possible values are all (default), photos or videos
*)

  (* CRv2 jfuruse: todo: Fancy lazy loading *)
  let getPhotos photoset_id o =
    let open GetPhotos in
    let rec f n st =
      raw_getPhotos photoset_id o ~page:n 
      >>= fun { photoset = pset } ->
        if pset.per_page * n > pset.total then
          (* last page *)
          `Ok { pset with photo = st @ pset.photo; }
        else f (n+1) (st @ pset.photo)
    in
    f 1 []


  let removePhotos photoset_id photo_ids o =
    json_api ~post:true o "flickr.photosets.removePhotos"
      [ "api_key", App.app.Xoauth.Consumer.key
      ; "photoset_id", photoset_id
      ; "photo_ids", String.concat "," photo_ids
      ]
    >>= Fail.check
    >>= fun _ -> return ()
    
end

module People = struct

  module GetUploadStatus = struct

    module Bandwidth = struct
      type t = { 
        max : int64;
        used : int64;
        maxbytes : int64;
        usedbytes : int64;
        remainingbytes : int64;
        maxkb : int64;
        usedkb : int64;
        remainingkb : int64;
        unlimited : ibool
      } with conv(json,ocaml)
    end

    module Filesize = struct
      type t = {
        max : int64;
        maxbytes : int64;
        maxkb : int64;
        maxmb : int64;
      } with conv(json,ocaml)
    end

    module Videosize = struct
      type t = {
        maxbytes : int64;
        maxkb : int64;
        maxmb : int64;
      } with conv(json,ocaml)
    end

    module Sets = struct
      type t = { 
        created : sint;
        remaining : string
      } with conv(json,ocaml)
    end

    module Videos = struct
      type t = { 
        uploaded : sint;
        remaining : string
      } with conv(json,ocaml)
    end

    type t = { 
      user : user;
      stat : string 
    }

    and user = { 
      id : string;
      ispro : ibool;
      username : content;
      bandwidth : Bandwidth.t;
      filesize : Filesize.t;
      sets : Sets.t;
      videosize : Videosize.t;
      videos : Videos.t
    } with conv(json,ocaml)
  end

  let getUploadStatus o =
    json_api o "flickr.people.getUploadStatus"
      [ "api_key", App.app.Xoauth.Consumer.key
      ]
    >>= Fail.check
    >>= lift_error GetUploadStatus.t_of_json

end
