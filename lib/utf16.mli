val utf16c_to_utf8c : string -> string
(** Convert utf16 char code string to its utf8 

    [utf16c_to_utf8c "FFFF" = "EFBFBF"]
    [utf16c_to_utf8c "10ffff" = "1EFBFBF"]
*)

