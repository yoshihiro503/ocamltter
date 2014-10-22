open Util

let (>>) = (lsr)

(* "0000" <= x <= "10ffff" *)
let utf16c_to_utf8c (x) =
  let n = int_of_string("0x" ^ x) in
  if n < 0x80 then
    to_hex n
  else begin
    (if n < 0x800 then
      to_hex((n >> 6) land 0x1f lor 0xc0)
    else
      to_hex ((n >> 12) lor 0xe0) ^ to_hex((n >> 6) land 0x3f lor 0x80))
    ^ to_hex (n land 0x3f lor 0x80)
  end
