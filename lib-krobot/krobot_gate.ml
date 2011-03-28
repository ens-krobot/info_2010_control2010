(*
 * krobot_gate.ml
 * --------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt

open Krobot_dbus_gate.Fr_krobot_Service_Gate

let proxy krobot = OBus_proxy.make (OBus_peer.make (Krobot.to_bus krobot) "fr.krobot.Service.Gate") ["fr"; "krobot"; "Services"; "Gate"]

let enable krobot =
  OBus_method.call m_enable (proxy krobot) ()

let disable krobot =
  OBus_method.call m_disable (proxy krobot) ()

let close krobot =
  OBus_method.call m_close (proxy krobot) ()

let open_ krobot =
  OBus_method.call m_open (proxy krobot) ()

let hold_closed krobot =
  OBus_method.call m_hold_closed (proxy krobot) ()

let stop krobot =
  OBus_method.call m_stop (proxy krobot) ()
