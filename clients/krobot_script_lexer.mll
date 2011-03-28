(*
 * krobot_script_lexer.mll
 * -----------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

{
  module Text_set = Set.Make(Text)

  exception Parse_failure of string
}

let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper
let digit = ['0'-'9']
let alnum = alpha | digit
let punct = ['!' '"' '#' '$' '%' '&' '\'' '(' ')' '*' '+' ',' '-' '.' '/' ':' ';' '<' '=' '>' '?' '@' '[' '\\' ']' '^' '_' '`' '{' '|' '}' '~']
let graph = alnum | punct
let print = graph | ' '
let blank = ' ' | '\t'
let cntrl = ['\x00'-'\x1F' '\x7F']
let xdigit = digit | ['a'-'f' 'A'-'F']
let space = blank | ['\n' '\x0b' '\x0c' '\r']

let identstart = [ 'A'-'Z' 'a'-'z' '_' ]
let identbody = [ 'A'-'Z' 'a'-'z' '_' '-' '\'' '0' - '9' '.' ]
let ident = identstart identbody*
let maybe_ident = "" | ident

let value = (alpha | digit | "-" | "." | "_")+

rule partial_command = parse
  | blank* as before (maybe_ident as id) eof
      { `Command(before, id) }
  | blank* (ident as command) as s
      { let buf = Buffer.create 42 in
        Buffer.add_string buf s;
        let args, last = partial_arguments buf lexbuf in
        `Arg(Buffer.contents buf, command, args, last) }
  | ""
      { raise (Parse_failure "command expceted") }

and partial_arguments buf = parse
  | (blank+ as before) (maybe_ident as key) eof
      { Buffer.add_string buf before;
        (Text_set.empty, `Key key) }
  | (blank+ (ident as key) blank* "=" blank* as before) ((value | "") as value) eof
      { Buffer.add_string buf before;
        (Text_set.empty, `Value(key, value)) }
  | blank+ (ident as key) blank* "=" blank* value as s
      { Buffer.add_string buf s;
        let set, x = partial_arguments buf lexbuf in
        (Text_set.add key set, x) }
  | ""
      { (Text_set.empty, `Nothing) }

and command = parse
  | blank* (ident as command)
      { (command, arguments lexbuf) }
  | ""
      { raise (Parse_failure "command expceted") }

and arguments = parse
  | blank+ (ident as key) blank* "=" blank* (value as value)
      { (key, value) :: arguments lexbuf }
  | blank* eof
      { [] }
  | "" { raise (Parse_failure "syntax error") }
