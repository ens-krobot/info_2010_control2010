(*
 * krobot_arg.ml
 * -------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt
open Lwt_term

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)


type section = string
type 'a opt = ?section : section -> key : string -> ?doc : string -> unit -> 'a option Lazy.t
type 'a opt_d = ?section : section -> key : string -> ?doc : string -> default : 'a -> unit -> 'a Lazy.t

type option_info = {
  section : section;
  key : string;
  doc : string;
  kind : string;
  spec : Arg.spec;
}

(* +-----------------------------------------------------------------+
   | Registrartion                                                   |
   +-----------------------------------------------------------------+ *)

let program_name = Filename.basename Sys.argv.(0)

let parsed = ref false
  (* Whether parsing has already been done *)

let options = ref []
  (* All registered options *)

let register value opt =
  options := opt :: !options;
  lazy(
    if !parsed then
      !value
    else
      Printf.ksprintf failwith "Yaop: option '%s' accessed before calling Krobot_arg.parse'" opt.key
  )

let make kind spec ?(section=program_name) ~key ?(doc="") () =
  let value = ref None in
  register value {
    section = section;
    key = key;
    doc = doc;
    kind = kind;
    spec = spec value;
  }

let make_d kind spec ?(section=program_name) ~key ?(doc="") ~default () =
  let value = ref default in
  register value {
    section = section;
    key = key;
    doc = doc;
    kind = kind;
    spec = spec value;
  }

let int = make "<integer>" (fun x -> Arg.Int(fun v -> x := Some v))
let int_d = make_d "<integer>" (fun x -> Arg.Int(fun v -> x := v))

let float = make "<float>" (fun x -> Arg.Float(fun v -> x := Some v))
let float_d = make_d "<float>" (fun x -> Arg.Float(fun v -> x := v))

let string = make "<string>" (fun x -> Arg.String(fun v -> x := Some v))
let string_d = make_d "<string>" (fun x -> Arg.String(fun v -> x := v))

let keyword_kind keywords =
  "{" ^ String.concat "|" (List.map fst keywords) ^ "}"

let keyword ~keywords =
  make
    (keyword_kind keywords)
    (fun x -> Arg.Symbol(List.map fst keywords, fun v -> x := Some(List.assoc v keywords)))
let keyword_d ~keywords =
  make_d
    (keyword_kind keywords)
    (fun x -> Arg.Symbol(List.map fst keywords, fun v -> x := List.assoc v keywords))

let flag = make "" (fun x -> Arg.Unit(fun () -> x := Some true))
let flag_d = make_d "" (fun x -> Arg.Unit(fun () -> x := true))

let nflag = make "" (fun x -> Arg.Unit(fun () -> x := Some false))
let nflag_d = make_d "" (fun x -> Arg.Unit(fun () -> x := false))

(* +-----------------------------------------------------------------+
   | Default options                                                 |
   +-----------------------------------------------------------------+ *)

let display_help = flag_d ~key:"-help" ~doc:"show this help and exit" ~default:false ()

(* +-----------------------------------------------------------------+
   | Help                                                            |
   +-----------------------------------------------------------------+ *)

let usage msg =
  Lwt_main.run
    (eprintc [text program_name; text ": "; fg lred; text msg; reset; text "\n";
              textf "Type `"; fg lblue; textf "%s -help" program_name; reset; text "' for help.\n"]);
  exit 2

module String_set = Set.Make(String)

let help () =
  Lwt_main.run begin
    lwt () =
      printc [bold; fg white; text "Usage: "; reset; textf "%s <options>\n" program_name;
              fg white; text "options are:\n\n"]
    in
    let max_len =
      List.fold_left
        (fun m opt ->
           max m (String.length opt.key + String.length opt.kind))
        0 !options
    in
    let sections =
      List.fold_left
        (fun set opt -> String_set.add opt.section set)
        String_set.empty !options
    in
    Lwt_list.iter_s
      (fun section ->
         let args = List.filter (fun opt -> opt.section = section) !options in
         lwt () = printc [bold; fg white; text "- "; text section; text ":\n"; reset] in
         lwt () =
           Lwt_list.iter_s
             (fun opt ->
                printc [text "    "; fg lmagenta; text opt.key; text " "; fg yellow; text opt.kind; text " "; reset;
                        text (Text.repeat (3 + max_len - String.length opt.key - String.length opt.kind) " ");
                        text opt.doc; text "\n"])
             args
         in
         printc [text "\n"])
      (String_set.elements sections)
  end;
  exit 0

(* +-----------------------------------------------------------------+
   | Parsing                                                         |
   +-----------------------------------------------------------------+ *)

let parse () =
  let complete = ref false in
  try
    Arg.parse_argv
      ~current:(ref 0) Sys.argv
      (("-complete", Arg.Set complete, "") :: List.map (fun opt -> (opt.key, opt.spec, opt.doc)) !options)
      ignore "";
    if !complete then begin
      print_endline (String.concat " " (List.map  (fun opt -> opt.key) !options));
      exit 0
    end;
    parsed := true;
    if Lazy.force display_help then help ();
  with
    | Arg.Help msg ->
        help ()
    | Arg.Bad msg ->
        match Text.split ~sep:"\n" ~max:2 msg with
          | line :: _ -> begin
              match Text.split ~sep:":" ~max:2 line with
                | [_; msg] ->
                    usage (Text.strip msg)
                | _ ->
                    usage line
            end
          | _ ->
              usage "command line parsing error"
