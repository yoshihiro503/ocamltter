(* application level library *)
open Ocaml_conv 
open Spotlib.Spot
open Twitter
open Util
open Auth

module App = struct
  type t = { name : string; consumer : Consumer.t } with conv(ocaml)

  let dummy1 = { name = "dummy app1";
                 consumer = Consumer.dummy }

  let dummy2 = { name = "dummy app2";
                 consumer = Consumer.dummy }
end

module User = struct
   type t = { token        : string;
              token_secret : string;
              verif        : string } with conv(ocaml)

  let dummy = { token = "123456789-Base64DataHereBase64DataHereBase64DataHe";
                token_secret = "Base64DataHereBase64DataHereBase64DataHer";
                verif = "9876543 (digit given from twitter auth page)" }
end

module Auth = struct

  type ('a,'b) hashtbl = ('a,'b) Hashtbl.t (* for conv(ocaml) *)

  type t = { app: App.t; users: (string, User.t) hashtbl } with conv(ocaml)
  type tbl = (string, t) Hashtbl.t

  let dummy1 = { app = App.dummy1;
                 users = Hashtbl.of_list 1 [ "dummy user", User.dummy ] }

  let dummy2 = { app = App.dummy2;
                 users = Hashtbl.of_list 1 [ "dummy user", User.dummy ] }

  let find tbl ~app ~user = 
    let t = Hashtbl.find tbl app in
    match Hashtbl.find_all t.users user with
    | [] -> raise Not_found
    | [user] -> t.app, user
    | _ -> failwithf "User %s has more than one entry for app %s" user app

  let oauth app user = 
    { Oauth.consumer_key  = app.App.consumer.Consumer.key;
      consumer_secret     = app.App.consumer.Consumer.secret;
      access_token        = user.User.token;
      access_token_secret = user.User.token_secret;
    }

  let load path = 
    let tbl = Hashtbl.create 17 in
    Ocaml.load_with_exn t_of_ocaml path
    |> List.iter (fun t -> 
      if Hashtbl.mem tbl t.app.App.name then
        failwithf "Auth file %s contains more than one entry of application %s" 
          path t.app.App.name;
      Hashtbl.add tbl t.app.App.name t)
    |> fun () -> tbl

  let save path tbl = 
    Hashtbl.to_list tbl
    |> List.map snd
    |> Ocaml.save_with ocaml_of_t ~perm:0o600 path

  let save_dummy path =
    if Sys.file_exists path then failwithf "%s: already exists" path;
    save path & Hashtbl.of_list 1 [dummy1.app.App.name, dummy1; 
                                   dummy2.app.App.name, dummy2]

  let authorize app (_, verif as verified_token : VerifiedToken.t) = 
    let app_consumer = app.App.consumer in
    match Auth.fetch_access_token app_consumer verified_token with
    | `Ok (username, token) ->
        let oauth = Auth.oauth app_consumer (token, verif) in
        username, { User.token = oauth.Oauth.access_token;
                    token_secret = oauth.Oauth.access_token_secret;
                    verif = verif }
    | `Error (`Http (st, err)) ->
        failwithf "oauth http failed(%d): %s" st err
  
  let authorize_interactive app = 
    match Auth.fetch_request_token app.App.consumer with
    | `Ok (url, req_resp_token) ->
        print_endline & "Please grant access to " ^ app.App.name ^ " and get a PIN at :";
        print_endline & "  " ^ url;
        print_string "Give me a PIN: "; flush stdout;
        let verif = read_line () in
        let username, t = authorize app (req_resp_token, verif) in
        print_endline ("Grant Success! Hello, @"^username^" !");
        username, t
    | `Error (`Http (st, err)) ->
        failwithf "oauth http failed(%d): %s" st err

end

module Single = struct
  (** It forgets username and consumer *)
  let save path t = 
    open_out_with path (fun ch ->
      output_string ch & String.concat "\n" [ t.User.token;
                                              t.User.token_secret;
                                              t.User.verif;
                                              "" ])

  let load path = open_in_with path & fun ch ->
    let token    = input_line ch in 
    let secret   = input_line ch in 
    let verif    = input_line ch in
    { User.token   = token;
      token_secret = secret;
      verif        = verif }

  let oauth path app = 
    let user = try load path with _ -> 
      let _username, t = Auth.authorize_interactive app in
      save path t;
      t
    in
    Auth.oauth app user
end

