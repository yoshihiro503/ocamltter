open Util
open Http

let play_command = "mpg123"

type lang = En | Ja
let slang = function
  | En -> "en"
  | Ja -> "ja"

let cmd s =
(*  print_endline s;*)
  ignore @@ Sys.command s

let agent = "Mozilla/5.0 (X11; U; Linux i686; ja; rv:1.9.2.11) Gecko/20101013 Ubuntu/9.04 (jaunty) Firefox/3.6.11"

let wget outfile url =
  cmd (!%"wget -O %s -U '%s' '%s' >> log 2>> log" outfile agent url)
    

let play file =
  cmd (!% "%s %s >> log 2>> log" play_command file)

let say lang s =
  let save fname ch =
    open_out_with fname (fun out ->
      let rec iter () =
        try output_char out (input_char ch); iter() with
        | End_of_file -> ()
      in
      iter ())
  in
  let () = Http.conn "translate.google.com" Http.GET
      ~headers:[("User-Agent", agent)]
      "/translate_tts"
    [("tl", slang lang); ("q",s)]
    (fun _ ch -> save "say.mp3" ch)
  in
(*  let url =
    !%"http://translate.google.com/translate_tts?tl=%s&q=%s" (slang lang) s
  in
  wget "say.mp3" url;*)
  play "say.mp3"

type table = (string * string) list

let say_ja table s =
  let s' =
    List.fold_left (fun s (a,b) ->
      Str.global_replace (Str.regexp_string a) b s) s table
  in
  say Ja s'
