(*
 * krobot_turret.ml
 * ----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt

open Krobot_dbus_turret.Fr_krobot_Service_Turret

let proxy krobot = OBus_proxy.make (OBus_peer.make (Krobot.to_bus krobot) "fr.Krobot.Service.Turret") ["fr"; "krobot"; "Services"; "Turret"]

let goto krobot ~angle =
  OBus_method.call m_goto (proxy krobot) angle

let scan krobot =
  OBus_method.call m_scan (proxy krobot) ()

let find krobot =
  OBus_method.call m_find (proxy krobot) ()
