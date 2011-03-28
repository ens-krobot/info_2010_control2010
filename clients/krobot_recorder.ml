(*
 * krobot_recorder.ml
 * ------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Records all messages that goes through the krobot bus *)

open Lwt

let count = ref 0

let filter message =
  incr count;
  ignore (
    lwt () = Lwt_io.eprintf "\r%d messages received" !count in
    let str, _ = OBus_wire.string_of_message message in
    Lwt_io.print str
  );
  Some message

lwt () =
  Krobot_arg.parse ();
  lwt bus = Krobot_dbus.open_bus () in
  lwt () = Lwt_io.eprint "0 messages received" in
  let _ = Lwt_sequence.add_r filter (OBus_connection.incoming_filters bus) in
  lwt () =
    Lwt_list.iter_p
      (fun typ -> OBus_bus.add_match bus (OBus_match.rule ~typ ()))
      [`Method_call; `Method_return; `Error; `Signal]
  in
  fst (wait ())
