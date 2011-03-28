(*
 * script.ml
 * ---------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt
open Lwt_term
open OBus_value
open OBus_introspect_ext

let section = Lwt_log.Section.make "script"

module Text_set = Set.Make(Text)

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

type logger = Lwt_term.styled_text -> unit Lwt.t

type command = {
  cmd_path : string list;
  (* The name of the command with its path *)

  cmd_iargs : (string * single * string list) list;
  (* List of input arguments, with their types and constants they accept *)

  cmd_oargs : (string * single * string list) list;
  (* List of output arguments, with their types and constants they accept *)

  cmd_annotations : (string * string) list;
  (* List of annotations of the method *)

  cmd_proxy : OBus_proxy.t;
  (* The proxy on which the command should be executed *)

  cmd_method : (V.sequence, V.sequence) OBus_member.Method.t;
  (* The corresponding D-Bus method *)
}

module String_map = Map.Make(String)

type interpreter = {
  itp_krobot : Krobot.t;
  itp_commands : command String_map.t React.signal;
  itp_set_commands : command String_map.t -> unit;
  mutable itp_monitor_names : unit Lwt.t React.event;
}

(* +-----------------------------------------------------------------+
   | D-Bus extended introspection --> commands                       |
   +-----------------------------------------------------------------+ *)

(* Convert a D-Bus name into a command name *)
let convert_name name =
  Text.concat "."
    (List.map
       (fun elt ->
          Text.concat "-"
            (List.map
               String.lowercase
               (List.flatten
                  (List.map
                     OBus_name.split
                     (Text.split ~sep:"_" elt)))))
       (Text.split ~sep:"." name))

let convert_arguments args =
  let make_constants = function
    | Basic(Enum(typ, l)) -> List.map snd l
    | _ -> []
  in
  let rec loop i = function
    | [] ->
        []
    | (Some name, typ) :: rest ->
        (convert_name name, typ, make_constants typ) :: loop (i + 1) rest
    | (None, typ) :: rest ->
        ("_" ^ string_of_int i, typ, make_constants typ) :: loop (i + 1) rest
  in
  loop 0 args

let add_commands_of_interface proxy path (iface_name, members, env, annotations) acc =
  let rec loop acc = function
    | [] ->
        acc
    | Method(name, isig, osig, annotations) :: rest ->
        let isig = List.map (fun (name, term) -> (name, resolve env term)) isig
        and osig = List.map (fun (name, term) -> (name, resolve env term)) osig in
        let path = path @ [convert_name name] in
        loop
          (String_map.add (Text.concat "." path)
             { cmd_path = path;
               cmd_iargs = convert_arguments isig;
               cmd_oargs = convert_arguments osig;
               cmd_annotations = annotations;
               cmd_proxy = proxy;
               cmd_method = (
                 OBus_member.Method.make
                   ~interface:iface_name
                   ~member:name
                   ~i_args:(arguments
                              ~arg_types:(C.dyn_sequence (strip_sequence (List.map snd isig)))
                              ~arg_names:(List.map fst isig))
                   ~o_args:(arguments
                              ~arg_types:(C.dyn_sequence (strip_sequence (List.map snd osig)))
                              ~arg_names:(List.map fst osig))
                   ~annotations
               )} acc)
          rest
    | _ :: rest ->
        loop acc rest
  in
  loop acc members

(* +-----------------------------------------------------------------+
   | Service monitoring                                              |
   +-----------------------------------------------------------------+ *)

let collect interpreter name path prefix =
  try_lwt
    let peer = OBus_peer.make (Krobot.to_bus interpreter.itp_krobot) name in
    lwt _, children = OBus_proxy.introspect (OBus_proxy.make peer prefix) in
    lwt commands =
      Lwt_list.map_p
        (fun child ->
           let proxy = OBus_proxy.make peer (prefix @ [child]) in
           lwt interfaces, _ = OBus_proxy.introspect proxy in
           let path = path @ [convert_name child] in
           return (proxy, path, interfaces))
        children
    in
    interpreter.itp_set_commands
      (List.fold_left
         (fun acc (proxy, path, interfaces) ->
            List.fold_left
              (fun acc ((iface_name, _, _) as interface) ->
                 if Text.starts_with iface_name "fr.krobot." then
                   add_commands_of_interface proxy path (decode interface) acc
                 else
                   acc)
              acc interfaces)
         (React.S.value interpreter.itp_commands) commands);
    return ()
  with exn ->
    Lwt_log.error_f ~section ~exn "failed to collect objects from %S" name

let add_service interpreter name =
  match Text.split ~sep:"." name with
    | ["fr"; "krobot"; "Service"; _] ->
        collect interpreter name [] ["fr"; "krobot"; "Services"]
    | ["fr"; "krobot"; "Driver"] ->
        lwt () = collect interpreter name ["unsafe"] ["fr"; "krobot"; "Devices"]
        and () = collect interpreter name ["cards"] ["fr"; "krobot"; "Cards"] in
        return ()
    | _ ->
        return ()

let monitor_names interpreter (name, old_owner, new_owner) =
  interpreter.itp_set_commands
    (String_map.fold
       (fun cmd_name command acc ->
          if OBus_proxy.name command.cmd_proxy = name then
            acc
          else
            String_map.add cmd_name command acc)
       (React.S.value interpreter.itp_commands)
       String_map.empty);
  match new_owner with
    | "" ->
        return ()
    | _ ->
        add_service interpreter name

(* +-----------------------------------------------------------------+
   | Interpreter creation                                            |
   +-----------------------------------------------------------------+ *)

let command_equal cmd1 cmd2 =
  cmd1.cmd_path = cmd2.cmd_path && cmd1.cmd_iargs = cmd2.cmd_iargs

let make krobot =
  let commands, set_commands = React.S.create ~eq:(String_map.equal command_equal) String_map.empty in
  let rec interpreter = {
    itp_krobot = krobot;
    itp_commands = commands;
    itp_set_commands = set_commands;
    itp_monitor_names = React.E.never;
  } in
  lwt event = OBus_signal.connect (OBus_bus.name_owner_changed (Krobot.to_bus krobot)) in
  interpreter.itp_monitor_names <- React.E.map (monitor_names interpreter) event;
  lwt names = OBus_bus.list_names (Krobot.to_bus krobot) in
  lwt () = Lwt_list.iter_p (add_service interpreter) names in
  return interpreter

(* +-----------------------------------------------------------------+
   | Completion                                                      |
   +-----------------------------------------------------------------+ *)

let complete (before, after) commands =
  try
    match Krobot_script_lexer.partial_command (Lexing.from_string before) with
      | `Command(before, name) ->
          let path, last = match Text.rev_split ~sep:"." ~max:2 name with
            | [] -> ("", "")
            | [last] -> ("", last)
            | [path; last] -> (path ^ ".", last)
            | _ -> assert false
          in
          let n = Text.count ((=) ".") path in
          let names =
            String_map.fold
              (fun name' cmd acc ->
                 if Text.starts_with name' name then
                   Text_set.add (List.nth cmd.cmd_path n) acc
                 else
                   acc)
              commands Text_set.empty
          in
          Lwt_read_line.complete ~suffix:"" (before ^  path) last after names

      | `Arg(before, name, args, `Key key) ->
          let cmd = String_map.find name commands in
          let args' = List.fold_left (fun set (name, _, _) -> Text_set.add name set) Text_set.empty cmd.cmd_iargs in
          (* Remove already passed arguments *)
          let args = Text_set.diff args' args in
          Lwt_read_line.complete ~suffix:"=" before key after args

      | `Arg(before, name, args, `Value(key, value)) ->
          let cmd = String_map.find name commands in
          let name, typ, constants = List.find (fun (name, typ, constants) -> name = key) cmd.cmd_iargs in
          Lwt_read_line.complete ~suffix:" " before value after
            (List.fold_left (fun set constant -> Text_set.add constant set) Text_set.empty constants)

      | `Arg(before, name, args, `Nothing) ->
          raise Exit

  with Exit | Not_found | Krobot_script_lexer.Parse_failure _ ->
    { Lwt_read_line.comp_state = (before, after);
      Lwt_read_line.comp_words = Text_set.empty }

let completion interpreter edition_state =
  React.S.l2 complete edition_state interpreter.itp_commands

(* +-----------------------------------------------------------------+
   | Execution                                                       |
   +-----------------------------------------------------------------+ *)

exception Arg_error of string * string

let rec basic_of_string typ str =
  match typ with
    | Byte ->
        if String.length str = 1 then
          V.Byte(str.[0])
        else
          failwith "latin-1 character expected"
    | Boolean ->
        V.Boolean(bool_of_string str)
    | Int16 ->
        V.Int16(int_of_string str)
    | Uint16 ->
        V.Uint16(int_of_string str)
    | Int32 ->
        V.Int32(Int32.of_string str)
    | Uint32 ->
        V.Uint32(Int32.of_string str)
    | Int64 ->
        V.Int64(Int64.of_string str)
    | Uint64 ->
        V.Uint64(Int64.of_string str)
    | Double ->
        V.Double(float_of_string str)
    | String ->
        V.String str
    | Object_path ->
        V.Object_path (OBus_path.of_string str)
    | Signature ->
        V.Signature (signature_of_string str)
    | Enum(typ, l) ->
        let rec loop = function
          | [] ->
              basic_of_string (project_basic typ) str
          | (v, n) :: _ when n = str ->
              v
          | _ :: rest ->
              loop rest
        in
        loop l
    | Flag _ ->
        failwith "cannot yet parse flags"
    | Unix_fd ->
        failwith "how i am supposed to parse a file descriptor ?"

let sequence_of_args cmd args =
  List.map
    (fun (name, typ, constants) ->
       try
         match typ with
           | Basic typ ->
               V.basic
                 (basic_of_string typ
                    (try
                       List.assoc name args
                     with Not_found ->
                       List.assoc ("default." ^ name) cmd.cmd_annotations))
           | _ ->
               failwith "cannot yet parse D-Bus container values"
       with
         | Failure msg -> raise (Arg_error(name, msg))
         | Not_found -> raise (Arg_error(name, "missing value"))
         | exn -> raise (Arg_error(name, Printexc.to_string exn)))
    cmd.cmd_iargs

let exec ~interpreter ~logger ~command =
  match try `OK(Krobot_script_lexer.command (Lexing.from_string command)) with exn -> `Fail exn with
    | `Fail(Krobot_script_lexer.Parse_failure msg) ->
        logger [fg lred; textf "parse failure: %s" msg]
    | `Fail exn ->
        logger [fg lred; textf "parse failure: %s" (Printexc.to_string exn)]
    | `OK(name, args) ->
        match try Some(String_map.find name (React.S.value interpreter.itp_commands)) with Not_found -> None with
          | None ->
              logger [fg lred; textf "command '%s' does not exist or is currently unavailable" name]
          | Some cmd ->
              try_lwt
                lwt result = OBus_method.call cmd.cmd_method cmd.cmd_proxy (sequence_of_args cmd args) in
                lwt () = logger [textf "%s: " name] in
                let rec print values names =
                  match values, names with
                    | [], [] ->
                        return ()
                    | v :: lv, (name, _, _) :: ln ->
                        logger [textf "  %s = " name; text (V.string_of_single v)]
                    | _ ->
                        Printf.ksprintf failwith "invalid number of arguments retuned, expected %d, got %d"
                          (List.length cmd.cmd_oargs) (List.length result)
                in
                print result cmd.cmd_oargs
              with
                | OBus_error.DBus(_, msg) ->
                    logger [fg lred; textf "%s: %s" name msg]
                | Arg_error(arg, msg) ->
                    logger [fg lred; textf "%s: invalid argument %s: %s" name arg msg]
                | exn ->
                    logger [fg lred; textf "command '%s' failed with: %s" name (Printexc.to_string exn)]
