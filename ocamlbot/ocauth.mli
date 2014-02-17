(** OCamllter application level auth library *)

open Twitter
open Auth

(* application level library *)

module App : sig
  type t = { name : string; consumer : Consumer.t } with conv(ocaml)
  val dummy1 : t (** dummy sample data *)
  val dummy2 : t (** dummy sample data *)
end

module User : sig
  type t = { token : string; token_secret : string; verif : string; } with conv(ocaml)
  val dummy : t
end

module Auth : sig

  (** Full auth information *)    
  (* CR jfuruse: t should not be exposed. tbl has a bad name.. *)
  type t = { app: App.t; users: (string, User.t) Hashtbl.t } with conv(ocaml)

  type tbl = (string, t) Hashtbl.t
  (** Key is app-name *)

  val find : tbl -> app:string -> user:string -> App.t * User.t

  val oauth : App.t -> User.t -> Oauth.t

  val authorize : App.t -> VerifiedToken.t -> string * User.t

  val authorize_interactive : App.t -> string (** username *) * User.t

  val load : string -> tbl
  val save : string -> tbl -> unit
  val save_dummy : string -> unit
end 

module Single : sig
  val save : string -> User.t -> unit
  val load : string -> User.t
  val oauth : string -> App.t -> Oauth.t
end
