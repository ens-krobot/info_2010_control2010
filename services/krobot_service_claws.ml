(*
 * krobot_service_claws.ml
 * -----------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt
open Krobot_driver
open Krobot_dbus_claws.Fr_krobot_Service_Claws

type t = {
  obus : t OBus_object.t;
  krobot : Krobot.t;
}

let enable obj =
  Analogic_servos.set_config (analogic_servos obj.krobot) ~enable:[2; 4] ~disable:[]

let disable obj =
  Analogic_servos.set_config (analogic_servos obj.krobot) ~enable:[] ~disable:[0; 1; 2; 3; 4]

let open_ obj =
 Analogic_servos.set_state (analogic_servos obj.krobot) [(2, 18); (4, -69)]

let close obj =
 Analogic_servos.set_state (analogic_servos obj.krobot) [(2, -100); (4, 45)]

let take obj =
 Analogic_servos.set_state (analogic_servos obj.krobot) [(2, -40); (4, -20)]

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
    m_open = (
      fun obj () ->
        open_ (OBus_object.get obj)
    );
    m_close = (
      fun obj () ->
        close (OBus_object.get obj)
    );
    m_take = (
      fun obj () ->
        take (OBus_object.get obj)
    );
  }

let init bus =
  lwt krobot = Krobot.create () in
  let obj = {
    obus = OBus_object.make ~interfaces:[interface] ["fr"; "krobot"; "Services"; "Claws"];
    krobot = krobot;
  } in
  OBus_object.attach obj.obus obj;
  OBus_object.export bus obj.obus;
  return ()

lwt () = Krobot_daemon.start ~desc:"claws control" ~name:"fr.krobot.Service.Claws" init
