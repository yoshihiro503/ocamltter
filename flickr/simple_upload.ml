open Api

let auth_file = "ocaml_flickr.auth"

let o = Oauth.get_oauth auth_file

let () = 
  Arg.parse [] (fun p -> 
    prerr_endline "Upload test...";
    Upload.upload p o 
    |> fail_at_error 
    |> prerr_endline) "simple_upload <files>"
