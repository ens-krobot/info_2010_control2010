(*
 * krobot_dbus.ml
 * --------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

let section = Lwt_log.Section.make "dbus"

open Lwt

let dbus_command =
  Krobot_arg.string
    ~section:"D-Bus connection"
    ~key:"-dbus-command"
    ~doc:"command to connect to the krobot bus"
    ()

let dbus_address =
  Krobot_arg.string
    ~section:"D-Bus connection"
    ~key:"-dbus-address"
    ~doc:"address to connect to the krobot bus"
    ()

let connect_address str =
  lwt () = Lwt_log.info_f ~section "connecting to the krobot bus with address %S" str in
  OBus_bus.of_addresses (OBus_address.of_string str)

let connect_command str =
  lwt () = Lwt_log.info_f ~section "connecting to the krobot with command %S" str in
  let process = Lwt_process.open_process (Lwt_process.shell str) in
  let transport =
    OBus_transport.make
      ~send:(fun msg -> OBus_wire.write_message process#stdin msg)
      ~recv:(fun () -> OBus_wire.read_message process#stdout)
      ~shutdown:(fun () -> process#close >> return ())
      ()
  in
  let connection = OBus_connection.of_transport transport in
  lwt () = OBus_bus.register_connection connection in
  return connection

let bus = lazy(
  match Lazy.force dbus_command with
    | Some str ->
        connect_command str
    | None ->
        match Lazy.force dbus_address with
          | Some str ->
              connect_address str
          | None ->
              match try Some(Sys.getenv "DBUS_KROBOT_BUS_COMMAND") with Not_found -> None with
                | Some str ->
                    connect_command str
                | None ->
                    match try Some(Sys.getenv "DBUS_KROBOT_BUS_ADDRESS") with Not_found -> None with
                      | Some str ->
                          connect_address str
                      | None ->
                          connect_address "unix:abstract=krobot"
)

let open_bus () = Lazy.force bus
