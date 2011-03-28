(*
 * krobot_forward_dbus.ml
 * ----------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt

let rec copy ta tb =
  lwt msg = OBus_transport.recv ta in
  lwt () = OBus_transport.send tb msg in
  copy ta tb

lwt () =
  lwt (_, ta) = OBus_transport.of_addresses (OBus_address.of_string "unix:abstract=krobot") in
  let tb =
    OBus_transport.make
      ~send:(fun msg -> OBus_wire.write_message Lwt_io.stdout msg)
      ~recv:(fun () -> OBus_wire.read_message Lwt_io.stdin)
      ~shutdown:return
      ()
  in
  try_lwt
    copy ta tb <&> copy tb ta
  with End_of_file ->
    return ()
