open Spotlib.Spot
open OCamltter_oauth

type 'json mc_leftovers = (string * 'json) list [@@deriving conv{ocaml}]

module Oauth : sig
  include module type of struct include Oauth_ex.Make(Conf) end

  val load_acc_token : string -> Access_token.t
  (** Load an access token stored in the specified file.
      It raises an exception if the loading fails.
  *)

  val get_acc_token  : string -> Access_token.t
  (** Load an access token stored in the specified file.
      If the loading fails then it interactively authorize the access
      using CLI then save the new access token to the file, then returns it.
  *)
    
  val get_oauth      : string -> t
  (** Load the access token in the file then returns the OAuth info.
      If the loading fails then it interactively authorize the access
      using CLI then save the new access token to the file, then returns
      the OAuth info of it 
  *)
end

val raw_api :
  Oauth.t ->
  [< `GET | `POST ] ->
  string ->
  (string * string) list ->
  (string, [> Http.error ]) Result.t
(** The raw API caller *)

module Json : sig
  include module type of struct include Tiny_json.Json end

  type error = 
    | NotObject    of t
    | InvalidField of string
    | CastErr      of string
    | UnknownErr   of string
    | NotJsonErr   of exn
    | NoJsonResponse
  [@@deriving conv{ocaml_of}]

  val parse : string -> (t, [> `Json of error * string ]) Result.t

end

module Fail : sig
  type t = < code : int;
             message : string; 
             stat : string >
  [@@deriving conv{ocaml; json}]
  val format : Format.formatter -> t -> unit
  val check : Json.t -> [> `Error of [> `API of t ] | `Ok of Json.t ]
end

(** We see lots of records with only one field "content". 
    [Content.t] provides a wrapper of these records and OCaml string.
*)
module Content : sig
  type raw_content = < content : string > [@@deriving conv{json}]
  type t = string [@@deriving conv{ocaml; json}]
end
    
module EmptyResp : sig
  val check
    : Json.t
    -> (unit, [> `Json_conv of Json.t Meta_conv.Error.t ]) Result.t
  (** Check the JSON is a simple record {stat=x} where x <> "fail" *)
end

module Error : sig
  type t = [ `API of Fail.t
           | `Curl of Curl.curlCode * int * string
           | `Http of int * string
           | `Json of Json.error * string
           | `Json_conv of Json.t Meta_conv.Error.t ]
  (** Common errors *)
end

val json_api :
  Oauth.t ->
  [< `GET | `POST ] ->
  string ->
  (string * string) list ->
  (Json.t,
   [> `API of Fail.t
    | `Curl of Curl.curlCode * int * string
    | `Http of int * string
    | `Json of Json.error * string ])
  Result.t

type ('a, 'error) result = ('a, ([> Error.t] as 'error)) Result.t
(** Common result *)

module Auth : sig
  module Oauth : sig
    val checkToken : Oauth.t -> (Json.t, 'error) result
    (** flickr.auth.oauth.checkToken *)
  end
end

module TagList : sig
  type t = string list [@@deriving conv{ocaml; json}]
end

module Photos : sig

  module GetNotInSet : sig

    type photos =
        < page : int; 
          pages : int; 
          perpage : int; 
          photo : photo list; 
          total : int 
        >
    and photo = 
        < farm : int
        ; id : string
        ; isfamily : bool
        ; isfriend : bool
        ; ispublic : bool
        ; owner : string
        ; secret : string
        ; server : string
        ; tags : TagList.t option
        ; title : string 
        >
    [@@deriving conv{ocaml; json}]
  end

  val raw_getNotInSet :
      per_page:int ->
      page:int ->
      ?tags:bool ->
      Oauth.t ->
      (GetNotInSet.photos, 'error) result
  (** flickr.photos.getNotInSet *)

  val getNotInSet :
    ?tags:bool ->
    Oauth.t ->
    (< pages : int
    ; perpage : int
    ; stream : ((GetNotInSet.photo,
                 ([> Error.t] as 'error) * (int -> 'a Stream.t) * int) Result.t as 'a) Stream.t
    ; total : int 
      >,
     'error) Result.t
  (** flickr.photos.getNotInSet, stream interface *)

  module GetInfo : sig
    type owner =
        < location : string
        ; nsid : string
        ; realname : string
        ; unknowns : Json.t mc_leftovers
        ; username : string 
        >
    and dates =
      < posted : int64
      ; taken : string
      ; unknowns : Json.t mc_leftovers 
      >
    and urls = 
      < url : type_content list >
    and type_content = 
      < content : string
      ; type_ : string 
      >
    and photo =
      < comments : Content.t
      ; dates : dates
      ; dateuploaded : int64
      ; description : Content.t
      ; farm : int
      ; id : string
      ; isfavorite : bool
      ; license : int
      ; media : string
      ; originalformat : string
      ; originalsecret : string
      ; owner : owner
      ; rotation : int
      ; safety_level : int
      ; secret : string
      ; server : string
      ; title : Content.t
      ; unknowns : Json.t mc_leftovers
      ; urls : urls
      ; views : int
      ; visibility : visibility 
      >
    and visibility =
      < isfamily : bool
      ; isfriend : bool
      ; ispublic : bool 
      >
    [@@deriving conv{ocaml; json}]
  end

  val getInfo :
    string ->
    Oauth.t ->
    (GetInfo.photo, 'error) result
  (** flickr.photos.getInfo *)

  module GetExif :  sig
    type exif =
      < clean : string option
      ; label : string
      ; raw : Content.t
      ; tag : string
      ; tagspace : string
      ; tagspaceid : int >
    and photo =
      < camera : string
      ; exif : exif list
      ; id : string
      ; secret : string
      ; unknown : Json.t mc_leftovers >
    [@@deriving conv{ocaml; json}]
  end

  val getExif :
    string ->
    Oauth.t ->
    (GetExif.photo, 'error) result
  (** flickr.photos.getExif *)

  val addTags :
    string ->
    string list ->
    Oauth.t ->
    (unit, 'error) result
  (** flickr.photos.addTags *)

  val setTags :
    string ->
    string list ->
    Oauth.t ->
    (unit, 'error) result
  (** flickr.photos.setTags *)

  val delete :
    string ->
    Oauth.t ->
    (unit, 'error) result
  (** flickr.photos.delete *)

  module Search :
    sig
      type photos =
        < page : int
        ; pages : int
        ; perpage : int
        ; photo : photo list
        ; total : int >
      and photo =
        < farm : int
        ; id : string
        ; isfamily : bool
        ; isfriend : bool
        ; ispublic : bool
        ; owner : string
        ; secret : string
        ; server : string
        ; title : string 
        >
      [@@deriving conv{ocaml; json}]
    end

  val search :
    ?user_id:string ->
    ?tags:[< `All of string list | `Any of string list ] ->
    ?text:string ->
    ?per_page:int ->
    ?page:int ->
    Oauth.t ->
    (Search.photos, 'error) result
  (** flickr.photos.search *)

  val setPerms : string ->
    ?is_public:bool ->
    ?is_friend:bool ->
    ?is_family:bool ->
    ?perm_comment:[< `Contacts
                  | `Everybody
                  | `Friends_and_family
                  | `Nobody ] ->
    ?perm_addmeta:[< `Contacts
                  | `Everybody
                  | `Friends_and_family
                  | `Nobody ] ->
    Oauth.t ->
    (unit, 'error) result
  (** flickr.photos.setPerms *)
end

module Photosets : sig
  module Create : sig
    type photoset = 
      < id : string
      ; url : string 
      >
    [@@deriving conv{ocaml; json}]
  end

  val create :
    title:string ->
    primary_photo_id:string ->
    Oauth.t ->
    (Create.photoset, 'error) result
  (** flickr.photosets.create *)
      
  module GetList : sig
    type set =
      < can_comment : bool
      ; count_comments : int
      ; count_views : int
      ; date_create : string
      ; date_update : string
      ; description : Content.t
      ; farm : int
      ; id : string
      ; needs_interstitial : bool
      ; photos : int
      ; primary : string
      ; secret : string
      ; server : string
      ; title : Content.t
      ; videos : int
      ; visibility_can_see_set : bool 
      >
    and photoset =
      < cancreate : bool
      ; page : int
      ; pages : int
      ; perpage : int
      ; photoset : set list
      ; total : int 
      >
    and t = 
      < cancreate : bool
      ; photoset : set list
      ; total : int 
      >
    [@@deriving conv{ocaml_of; json}]
  end

  val raw_getList :
    per_page: int ->
    page:int ->
    Oauth.t ->
    (GetList.photoset, 'error) result
  (** flickr.photosets.getList *)

  val getList :
    Oauth.t ->
    (< cancreate : bool
     ; photoset : GetList.set list
     ; total : int >,
     'error) result
  (** flickr.photosets.getList, in stream api *)

  module GetPhotos : sig
    type photoset =
      < id : string
      ; owner : string
      ; ownername : string
      ; page : int
      ; pages : int
      ; per_page : int
      ; perpage : int
      ; photo : photo list
      ; primary : string
      ; title : string
      ; total : int 
      >
    and photo =
      < farm : int
      ; id : string
      ; isfamily : bool
      ; isfriend : bool
      ; isprimary : bool
      ; ispublic : bool
      ; secret : string
      ; server : string
      ; title : string 
      ; extras : Json.t mc_leftovers
      >
    [@@deriving conv{ocaml; json}]
  end

  val raw_getPhotos :
    string ->
    per_page: int ->
    page:int ->
    ?extras: string list ->
    Oauth.t ->
    (GetPhotos.photoset, 'error) result
  (** flickr.photosets.getPhotos 

      extras: extra information to fetch for each returned record. Currently supported fields are: license, date_upload, date_taken, owner_name, icon_server, original_format, last_update, geo, tags, machine_tags, o_dims, views, media, path_alias, url_sq, url_t, url_s, url_m, url_o
  *)

  val getPhotos :
    string ->
    ?extras: string list ->
    Oauth.t ->
    (< id : string
     ; owner : string
     ; ownername : string
     ; photo : GetPhotos.photo list
     ; primary : string
     ; title : string
     ; total : int >, 'error) result
  (** flickr.photosets.getPhotos, streamed *)

  val removePhotos :
    string ->
    string list ->
    Oauth.t ->
    (unit, 'error) result
  (** flickr.photosets.removePhotos *)

  val addPhoto :
    string ->
    photo_id:string ->
    Oauth.t ->
    (unit, 'error) result
  (** flickr.photosets.addPhoto *)

  val reorderPhotos : 
    string ->
    string list ->
    Oauth.t ->
    (unit, 'error) result 
end

module People : sig
  module GetUploadStatus :
    sig
      type bandwidth =
        < max : int64
        ; maxbytes : int64
        ; maxkb : int64
        ; remainingbytes : int64
        ; remainingkb : int64
        ; unlimited : bool
        ; used : int64
        ; usedbytes : int64
        ; usedkb : int64 >
      and filesize =
        < max : int64
        ; maxbytes : int64
        ; maxkb : int64
        ; maxmb : int64 
        >
      and videosize = 
        < maxbytes : int64
        ; maxkb : int64
        ; maxmb : int64 >
      and sets = 
        < created : int
        ; remaining : string >
      and videos = 
        < remaining : string
        ; uploaded : int >
      and user =
        < bandwidth : bandwidth
        ; filesize : filesize
        ; id : string
        ; ispro : bool
        ; sets : sets
        ; username : Content.t
        ; videos : videos
        ; videosize : videosize 
        >
      [@@deriving conv{ocaml; json}]
    end

  val getUploadStatus :
    Oauth.t ->
    (GetUploadStatus.user, 'error) result
  (** flickr.people.getUploadStatus *)
end

module Tags : sig
  module GetListPhoto :
    sig
      type photo = 
        < id : string
        ; tags : tags >
      and tags = < tag : t >
      and t = tag list
      and tag =
        < author : string
        ; authorname : string
        ; id : string
        ; machine_tag : bool
        ; raw : string
        ; tag : string >
      [@@deriving conv{ocaml; json}]
    end

  val getListPhoto :
    string ->
    Oauth.t ->
    (GetListPhoto.t, 'error) result
  (** flickr.tags.getListPhoto *)
end

module Test : sig

  module Login : sig
    type t = 
      < id : string
      ; username : Content.t >
    [@@deriving conv{ocaml; json}]
  end

  val login :
    Oauth.t ->
    (Login.t, 'error) result
  (** flickr.test.login *)
end
    
module Upload : sig

  val raw_api :
    Http.params ->
    string ->
    Oauth.t ->
    (string, [> Http.error ]) Result.t
  (** https://up.flickr.com/services/upload, raw api via XML *)

  val parse_rsp :
    Xml.xml -> 
    ( (string, 
       [> `API of < code : int
                  ; message : string
                  ; stat : string > ]) Result.t
    , string * Xml.xml) Result.t

  val upload :
      ?is_public:bool ->
      ?is_friend:bool ->
      ?is_family:bool ->
      ?hidden:bool ->
      ?title:string ->
      ?description:string ->
      ?tags:string list ->
      string ->
      Oauth.t ->
      (string,
       [> `API of < code : int
                  ; message : string
                  ; stat : string >
        | `Curl of Curl.curlCode * int * string
        | `Http of int * string
        | `XML_conv of string * Xml.xml
        | `XML_parse of string * exn ])
      Result.t
  (** https://up.flickr.com/services/upload *)
end

val format_error :
  Format.formatter ->
  [< `API of Fail.t
   | `Curl of Curl.curlCode * int * string
   | `Http of int * string
   | `Json of Json.error * string
   | `Json_conv of Json.t Meta_conv.Error.t
   | `Load of string * exn
   | `XML_conv of string * Xml.xml
   | `XML_parse of string * exn ] ->
  unit

val error :
  [< `API of Fail.t
   | `Curl of Curl.curlCode * int * string
   | `Http of int * string
   | `Json of Json.error * string
   | `Json_conv of Json.t Meta_conv.Error.t
   | `Load of string * exn
   | `XML_conv of string * Xml.xml
   | `XML_parse of string * exn ] ->
  'raise_exception

val fail_at_error :
  ('a,
   [< `API of Fail.t
   | `Curl of Curl.curlCode * int * string
   | `Http of int * string
   | `Json of Json.error * string
   | `Json_conv of Json.t Meta_conv.Error.t
   | `Load of string * exn
   | `XML_conv of string * Xml.xml
   | `XML_parse of string * exn ]) Result.t -> 'a
