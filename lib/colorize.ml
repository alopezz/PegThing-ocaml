
type color = Red | Green | Yellow | Blue | Magenta | Cyan


let fg_code_for_color color =
  Printf.sprintf "%s%s%s"
    "\027["
    (match color with
  | Red -> "31"
  | Green -> "32"
  | Yellow -> "33"
  | Blue -> "34"
  | Magenta -> "35"
  | Cyan -> "36")
    "m"

let bg_code_for_color color =
  Printf.sprintf "%s%s%s"
    "\027e["
    (match color with
     | Red -> "41"
     | Green -> "42"
     | Yellow -> "43"
     | Blue -> "44"
     | Magenta -> "45"
     | Cyan -> "46")
    "m"

let wrap_codes prefix postfix text =
  String.concat "" [prefix; text; postfix]

let wrap_fg color text =
  wrap_codes (fg_code_for_color color) "\027[39m" text

let wrap_bg color text =
  wrap_codes (bg_code_for_color color) "\027[49m" text


let colorize ?fg ?bg text =
  (match fg with
    | Some color -> wrap_fg color text
    | None -> text)
  |> (match bg with
    | Some color -> wrap_bg color
    | None -> Fun.id)
  
