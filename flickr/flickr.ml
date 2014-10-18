open Spotlib.Spot
open Twitter

open Ocaml_conv
open Json_conv

open Result

module Oauth = Oauth_ex.Make(struct
  let oauth_signature_method = `Hmac_sha1
  let oauth_callback = Some None (* oob *)
  let host = "www.flickr.com"
  let request_path = "/services/oauth/request_token"
  let access_path = "/services/oauth/access_token"
  let authorize_url = "https://www.flickr.com/services/oauth/authorize?oauth_token="
  let app = App.app
end)

type 'json mc_leftovers = (string * 'json) list with conv(ocaml)
type 'a mc_option = 'a option with conv(ocaml)

let load_auth auth_file =
  match Ocaml.load_with_exn Oauth.Access_token.t_of_ocaml auth_file with
  | [a] -> a
  | _ -> assert false

let get_acc_token auth_file =
  try load_auth auth_file with
  | _ -> 
      let _res, acc_token = Oauth.authorize_cli_interactive () in
      Ocaml.save_with Oauth.Access_token.ocaml_of_t ~perm:0o600 auth_file [acc_token];
      acc_token

let get_oauth auth_file =
  let acc_token = get_acc_token auth_file in
  Oauth.oauth Oauth.Conf.app acc_token

module Json = struct
  include Tiny_json.Json
(*
  let json_of_t x = x
  let t_of_json ?trace:_ x = `Ok x
*)
  let ocaml_of_t t = Ocaml.String (show t)
  let t_of_ocaml = Ocaml_conv.Helper.of_deconstr (function
    | Ocaml.String s -> parse s
    | _ -> failwith "Ocaml.String expected")
  let t_of_ocaml_exn = Ocaml_conv.exn t_of_ocaml
 
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

  type sint64 = int64 with conv(ocaml)

  let sint64_of_json ?(trace=[]) = function
    | (Number _ as j) -> int64_of_json ~trace j
    | (String s as t) -> 
        begin try `Ok (Int64.of_string s) with
        | e -> `Error (Meta_conv.Error.Exception e, t, `Node t :: trace)
        end
    | t -> `Error (Meta_conv.Error.Exception (Failure "Number or String expected"), t, `Node t :: trace)

  let json_of_sint64 = json_of_int64

  type ibool = bool with conv(ocaml)

  let ibool_of_json ?trace j = 
    sint_of_json ?trace j >>= fun n -> `Ok (n <> 0)

  let json_of_ibool b = Number (if b then "1" else "0")
end

open Json

let raw_api ?(post=false) o m fields = 
  Oauth.access `HTTPS o
    "api.flickr.com"
    "/services/rest"
    ~meth: (
      let xs = [ "method", m
               ; "format", "json" ] @ fields
      in
      if post then `POST xs else `GET xs
    )

let lift_error f s = match f s with
  | (`Ok _ as v) -> v
  | `Error e -> `Error (`Json_conv e)
    
module Fail = struct

  type t = < 
    stat : string (** "fail" *);
    code : sint;
    message : string 
  > with conv(json, ocaml)

  let format = Ocaml.format_with ocaml_of_t

  let check j =
    match lift_error t_of_json j with
    | `Ok e when e#stat = "fail" -> `Error (`API e)
    | _ -> `Ok j
end

(* Flickr's JSON response is always surrounded by

     jsonFlickrApi(<JSON>)
*)
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
      | "jsonFlickrApi(", s, ')' -> 
          (* Result has always the top field "stat" *)
          Json.parse s >>= Fail.check
      | exception _ -> `Error (`Json (Json.NoJsonResponse, s))
      | _ -> `Error (`Json (Json.NoJsonResponse, s))

type content = < content as "_content" : string > with conv(json, ocaml)

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
    with conv(json, ocaml)
  end
*)

  module GetNotInSet = struct
     
    type resp = < 
      photos : photos;
      stat   : string; 
    >

    and photos = <
      page : sint;
      pages : sint;
      perpage : sint;
      total : sint;
      photo : photo list;
    >

    and photo = <
      id : string;
      owner : string;
      secret : string;
      server : string;
      farm : sint;
      title : string;
      ispublic : ibool;
      isfriend : ibool;
      isfamily : ibool
    >
    with conv(json, ocaml)
  end

  let raw_getNotInSet ?(per_page=100) ?(page=1) (* ?privacy_filter ?media *) o = 
    assert (page > 0);
    json_api o "flickr.photos.getNotInSet"
      [ "api_key", App.app.Oauth.Consumer.key
      ; "per_page", string_of_int per_page
      ; "page", string_of_int page
      ]
    >>= lift_error GetNotInSet.resp_of_json
    >>| fun x -> x#photos

(*
max_upload_date (Optional)
Maximum upload date. Photos with an upload date less than or equal to this value will be returned. The date can be in the form of a unix timestamp or mysql datetime.
min_taken_date (Optional)
Minimum taken date. Photos with an taken date greater than or equal to this value will be returned. The date can be in the form of a mysql datetime or unix timestamp.
max_taken_date (Optional)
Maximum taken date. Photos with an taken date less than or equal to this value will be returned. The date can be in the form of a mysql datetime or unix timestamp.
min_upload_date (Optional)
Minimum upload date. Photos with an upload date greater than or equal to this value will be returned. The date can be in the form of a unix timestamp or mysql datetime.
extras (Optional)
A comma-delimited list of extra information to fetch for each returned record. Currently supported fields are: description, license, date_upload, date_taken, owner_name, icon_server, original_format, last_update, geo, tags, machine_tags, o_dims, views, media, path_alias, url_sq, url_t, url_s, url_q, url_m, url_n, url_z, url_c, url_l, url_o
page (Optional)
The page of results to return. If this argument is omitted, it defaults to 1.
*)

  module GetInfo = struct

    type owner = <
        nsid: string;
        username: string;
        realname: string;
        location: string;
        unknowns: Json.t mc_leftovers;
      >
    
    and dates = < 
        posted: sint64;
        taken: string;
        unknowns: Json.t mc_leftovers 
      >

    and urls = < url: type_content list >

    and type_content = < 
        type_ as "type": string; (* "photopage", *)
        content as "_content": string 
      >

    and resp = < photo: photo; stat: string >

    and photo = <
        id: string;
        secret : string;
        server : string;
        farm: sint;
        dateuploaded: sint64;
        isfavorite: ibool;
        license: sint;
        safety_level: sint;
        rotation: sint;
        originalsecret: string; (* "edf076f1f2" *)
        originalformat : string; (* "jpg" *)
        owner: owner;
        title: content;
        description: content;
        visibility: visibility;
        dates: dates;
        views: sint;
        comments: content;
        urls: urls;
        media: string; (* "photo" *)
        unknowns: Json.t mc_leftovers;
      >

    and visibility = < ispublic: ibool; isfriend: ibool; isfamily: ibool >

    with conv(json, ocaml)

  end
      

  let getInfo photo_id o =
    json_api o "flickr.photos.getInfo"
      [ "api_key", App.app.Oauth.Consumer.key
      ; "photo_id", photo_id
      ]
    >>= lift_error GetInfo.resp_of_json
    >>| fun x -> x#photo
    
(*
secret (Optional)
The secret for the photo. If the correct secret is passed then permissions checking is skipped. This enables the 'sharing' of individual photos by passing around the id and secret.

The <permissions> element is only returned for photos owned by the calling user. The isfavorite attribute only makes sense for logged in users who don't own the photo. The rotation attribute is the current clockwise rotation, in degrees, by which the smaller image sizes differ from the original image.

The <date> element's lastupdate attribute is a Unix timestamp indicating the last time the photo, or any of its metadata (tags, comments, etc.) was modified.
*)

  module GetExif = struct

    type exif = <
        tagspace   : string;
        tagspaceid : sint;
        tag        : string;
        label      : string;
        raw        : content;
        clean      : string mc_option
      >

    and resp = < photo : photo; stat : string >

    and photo = <
        id : string;
        secret : string;
        camera : string;
        exif: exif list;
        unknown : Json.t mc_leftovers 
      >
    with conv(json,ocaml)

  end
                  
  let getExif photo_id o =
    json_api o "flickr.photos.getExif"
      [ "api_key", App.app.Oauth.Consumer.key
      ; "photo_id", photo_id
      ]
    >>= lift_error GetExif.resp_of_json
    >>| fun x -> x#photo  

(*
secret (Optional)
The secret for the photo. If the correct secret is passed then permissions checking is skipped. This enables the 'sharing' of individual photos by passing around the id and secret.
*)

  let delete photo_id o = 
    json_api ~post:true o "flickr.photos.delete"
      [ "api_key", App.app.Oauth.Consumer.key
      ; "photo_id", photo_id
      ]
    >>| fun _ -> ()

end

module Photosets = struct

  module Create = struct

    type resp = < photoset: photoset; stat : string >

    and photoset = < id : string; url : string >
    with conv(json, ocaml)
  end

  let create ~title ~primary_photo_id o =
    json_api o "flickr.photosets.create"
      [ "api_key", App.app.Oauth.Consumer.key
      ; "title", title
      ; "primary_photo_id", primary_photo_id
      ]
    >>= lift_error Create.resp_of_json
    >>| fun x -> x#photoset
    
(*
description (Optional)
A description of the photoset. May contain limited html.
primary_photo_id (Required)
The id of the photo to represent this set. The photo must belong to the calling user.
Example Response

<photoset id="1234" url="http://www.flickr.com/photos/bees/sets/1234/" />
New photosets are automatically put first in the photoset ordering for the user. Use flickr.photosets.orderSets if you don't want the new set to appear first on the user's photoset list.

Error Codes

1: No title specified
No title parameter was passed in the request.
2: Photo not found
*)


  module GetList = struct

    type set = < id : string;
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
               >
  
    and photoset = < cancreate : ibool;
                     page : sint;
                     pages : sint;
                     perpage : sint;
                     total : sint;
                     photoset : set list >
  
    and resp = < photosets : photoset;
                 stat : string; >

    with conv(json, ocaml_of )

  end

  let raw_getList ?(page=1) o = 
    assert (page > 0);
    json_api o "flickr.photosets.getList"
      [ "api_key", App.app.Oauth.Consumer.key
      ; "page", string_of_int page
      ]
    >>= Fail.check
    >>= lift_error GetList.resp_of_json
    >>| fun x -> x#photosets

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
    let rec f n st =
      raw_getList o ~page:n 
      >>= fun psets ->
        if psets#perpage * n > psets#total then
          (* last page *)
          `Ok (object
            method cancreate = psets#cancreate
            method total = psets#total
            method photoset = st @ psets#photoset; 
          end)
        else f (n+1) (st @ psets#photoset)
    in
    f 1 []

  module GetPhotos = struct

    type resp = < photoset : photoset; stat : string; >
        
    and photoset = <
      id : string;
      primary : string;
      owner : string;
      ownername : string;
      photo : photo list;
      page : sint;
      per_page : sint;
      perpage : sint;
      pages : sint;
      total : sint;
      title : string;
    >

    and photo = <
      id : string;
      secret : string;
      server : string;
      farm : sint;
      title : string;
      isprimary : ibool;
      ispublic : ibool;
      isfriend : ibool;
      isfamily : ibool 
    >
    with conv(json,ocaml)

  end


  let raw_getPhotos photoset_id ?(page=1) o =
    assert (page > 0);
    json_api o "flickr.photosets.getPhotos"
      [ "api_key", App.app.Oauth.Consumer.key
      ; "photoset_id", photoset_id
      ; "page", string_of_int page
      ]
    >>= Fail.check
    >>= lift_error GetPhotos.resp_of_json
    >>| fun x -> x#photoset

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
    let rec f n st =
      raw_getPhotos photoset_id o ~page:n 
      >>= fun pset ->
        if pset#per_page * n > pset#total then
          (* last page *)
          `Ok (object
            method id        = pset#id
            method primary   = pset#primary
            method owner     = pset#owner
            method ownername = pset#ownername
            method photo     = st @ pset#photo
            method total     = pset#total
            method title     = pset#title
          end)
        else f (n+1) (st @ pset#photo)
    in
    f 1 []


  let removePhotos photoset_id photo_ids o =
    json_api ~post:true o "flickr.photosets.removePhotos"
      [ "api_key", App.app.Oauth.Consumer.key
      ; "photoset_id", photoset_id
      ; "photo_ids", String.concat "," photo_ids
      ]
    >>= Fail.check
    >>= fun _ -> return ()


  let addPhoto photoset_id ~photo_id o =
    json_api o "flickr.photosets.getPhotos"
      [ "api_key", App.app.Oauth.Consumer.key
      ; "photoset_id", photoset_id
      ; "photo_id", photo_id
      ]
    >>= Fail.check
    >>= fun _ -> `Ok ()

end

module People = struct

  module GetUploadStatus = struct

    type bandwidth = <
        max : int64;
        used : int64;
        maxbytes : int64;
        usedbytes : int64;
        remainingbytes : int64;
        maxkb : int64;
        usedkb : int64;
        remainingkb : int64;
        unlimited : ibool
      >

    and filesize = <
        max : int64;
        maxbytes : int64;
        maxkb : int64;
        maxmb : int64;
      >

    and videosize = <
        maxbytes : int64;
        maxkb : int64;
        maxmb : int64;
      >

    and sets = < 
        created : sint;
        remaining : string
      >

    and videos = <
        uploaded : sint;
        remaining : string
      >

    and resp = < 
      user : user;
      stat : string 
    >

    and user = < 
      id : string;
      ispro : ibool;
      username : content;
      bandwidth : bandwidth;
      filesize : filesize;
      sets : sets;
      videosize : videosize;
      videos : videos;
    > with conv(json,ocaml)
  end

  let getUploadStatus o =
    json_api o "flickr.people.getUploadStatus"
      [ "api_key", App.app.Oauth.Consumer.key
      ]
    >>= Fail.check
    >>= lift_error GetUploadStatus.resp_of_json
    >>| fun x -> x#user

end

module Upload = struct

  (* up.flickr.com does not support JSON response *)

  let raw_api fields img o = 
    Oauth.access `HTTPS o
      "up.flickr.com"
      "/services/upload"
      ~oauth_other_params: fields
      ~meth: (`POST_MULTIPART ["photo", `FILE img])

  let catch_with err f v = try `Ok (f v) with e -> `Error (err e)

  open Xml

  let tag p = function
    | Tag (name, _, _) as xml when p name -> [xml]
    | _ -> []

  let tag_named name = tag (fun x -> x = name)

  let pcdata = function
    | PCData _ as xml -> [xml]
    | _ -> []

  let children = function
    | Tag (_, _, xs) -> xs
    | PCData _ -> []
        
  let attr k = function
    | PCData _ -> []
    | Tag (_, attrs, _) -> 
        match List.assoc_opt k attrs with
        | None -> []
        | Some v -> [ PCData v ]
        
  let assoc_attr k = function
    | PCData _ -> None
    | Tag (_, attrs, _) -> List.assoc_opt k attrs
        
  let (^.) a b xml = List.concat_map a & b xml

  let parse_rsp xml =
    try
      match (attr "stat" ^. tag_named "rsp") xml with
      | [ PCData "ok" ] ->
          let photoids = (pcdata ^. children ^. tag_named "photoid" ^. children) xml in
          begin match photoids with
          | [] -> `Error ("<photoid> is not found", xml)
          | xs -> `Ok (`Ok (List.map (function PCData s -> s | _ -> assert false) xs))
          end
          
      | [ PCData "fail" ] ->
          begin match (tag_named "err" ^. children) xml with
          | [e] ->
              begin match assoc_attr "code" e, assoc_attr "msg" e with
              | Some code, Some msg -> 
                  begin try
                    `Ok (`Error (`API (object method stat = "fail" method code = int_of_string code method  message=msg end)))
                    with _ -> `Error ("error code is not int", xml)
                  end
                          
              | _ -> assert false
              end
          | [] -> assert false
          | _ -> assert false
          end
      | [ PCData s ] -> failwith s
      | [] -> assert false
      | _ -> assert false
    with
    | _ -> `Error ("rsp parse failed", xml)


  let upload 
      ?(is_public=false)
      ?(is_friend=false)
      ?(is_family=false)
      ?(hidden=true)
      img_file o =
        let bool k b = [k, if b then "1" else "0"] in
  	let fields = 
          List.concat 
            [ bool "is_public" is_public
  	    ; bool "is_friend" is_friend
  	    ; bool "is_family" is_family
  	    ; [ "hidden", if hidden then "2" else "1" ]
            ]
  	in
  	raw_api fields img_file o >>= fun s -> 
        catch_with (fun exn -> `XML_parse (s, exn)) Xml.parse_string s >>= fun xml ->
        match parse_rsp xml with
        | `Ok v -> v
        | `Error (mes, xml) -> `Error (`XML_conv (mes, xml))

        
(*
title (optional)
The title of the photo.
description (optional)
A description of the photo. May contain some limited HTML.
tags (optional)
A space-seperated list of tags to apply to the photo.
safety_level (optional)
Set to 1 for Safe, 2 for Moderate, or 3 for Restricted.
content_type (optional)
Set to 1 for Photo, 2 for Screenshot, or 3 for Other.
*)
    
end

let error = function
  | (`Http _ | `Curl _ as e) -> 
      !!% "HTTP: %s@." & Http.string_of_error e;
      assert false
  | `Json (e, s) -> 
      !!% "Json: %a@." Json.format_error e;
      !!% "  %S@." s;
      assert false
  | `Json_conv e ->
      !!% "Json_conv: %a@." Json_conv.format_full_error e;
      assert false
  | `API fail ->
      !!% "API: %a@." Fail.format fail;
      assert false
  | `Load (name, exn) ->
      !!% "Local file load failure: %s: %a@." name Exn.format exn;
      assert false
  | `XML_parse (s, exn) ->
      !!% "XML parse failure: %a : %s@." Exn.format exn s;
      assert false
  | `XML_conv (mes, xml) ->
      !!% "XML conv failure: %s : %s@." mes (Xml.show xml);
      assert false

