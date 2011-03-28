(*
 * krobot_grip.ml
 * --------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt

open Krobot_dbus_grip.Fr_krobot_Service_Grip

let proxy krobot = OBus_proxy.make (OBus_peer.make (Krobot.to_bus krobot) "fr.krobot.Service.Grip") ["fr"; "krobot"; "Services"; "Grip"]

let up krobot =
  OBus_method.call m_up (proxy krobot) ()

let down krobot =
  OBus_method.call m_down (proxy krobot) ()

let open_ krobot =
  OBus_method.call m_open (proxy krobot) ()

let close krobot =
  OBus_method.call m_close (proxy krobot) ()

let release krobot =
  OBus_method.call m_release (proxy krobot) ()
