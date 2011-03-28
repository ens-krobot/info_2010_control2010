(*
 * krobot_boardname.mll
 * --------------------
 * Copyright : (c) 2009, Stéphane Glondu <steph@glondu.net>
 * Licence   : BSD3
 *
 * This file is a part of Krobot.
 *)


let boardname_regex = ("Carte " | "Robot Interface" | "Battery Monitoring " | "Sensor Interface") [^'\n']+

rule boardname = parse
  | (boardname_regex as name) '\n' { Some name }
  | _ { boardname lexbuf }
  | eof { None }

{
  let get_board_name str =
    let lexbuf = Lexing.from_string str in
    boardname lexbuf
}
