(*
 * krobot_service_gate.ml
 * ----------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt
open Krobot_driver
open Krobot_dbus_gate.Fr_krobot_Service_Gate

type t = {
  obus : t OBus_object.t;
  krobot : Krobot.t;
}

let enable obj =
  Analogic_motor.set_state (analogic_motor obj.krobot) ~state:true

let disable obj =
  Analogic_motor.set_state (analogic_motor obj.krobot) ~state:false

let close obj =
  Analogic_motor.set_velocity (analogic_motor obj.krobot) ~velocity:200 ~duration:0.08

let open_ obj =
  Analogic_motor.set_velocity (analogic_motor obj.krobot) ~velocity:255 ~duration:0.1

let hold_closed obj =
  Analogic_motor.set_velocity (analogic_motor obj.krobot) ~velocity:200 ~duration:90.0

let stop obj =
  Analogic_motor.set_velocity (analogic_motor obj.krobot) ~velocity:0 ~duration:0.0

let interface =
  make {
    m_enable = (
      fun obj () ->
        enable (OBus_object.get obj)
    );
    m_disable = (
      fun obj () ->
        disable (OBus_object.get obj)
    );
    m_close = (
      fun obj () ->
        close (OBus_object.get obj)
    );
    m_open = (
      fun obj () ->
        open_ (OBus_object.get obj)
    );
    m_hold_closed = (
      fun obj () ->
        hold_closed (OBus_object.get obj)
    );
    m_stop = (
      fun obj () ->
        stop (OBus_object.get obj)
    );
  }

let init bus =
  lwt krobot = Krobot.create () in
  let obj = {
    obus = OBus_object.make ~interfaces:[interface] ["fr"; "krobot"; "Services"; "Gate"];
    krobot = krobot;
  } in
  OBus_object.attach obj.obus obj;
  OBus_object.export bus obj.obus;
  return ()

lwt () = Krobot_daemon.start ~desc:"gate control" ~name:"fr.krobot.Service.Gate" init
