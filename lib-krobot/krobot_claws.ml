(*
 * krobot_claws.ml
 * ---------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt

open Krobot_dbus_claws.Fr_krobot_Service_Claws

let proxy krobot = OBus_proxy.make (OBus_peer.make (Krobot.to_bus krobot) "fr.krobot.Service.Claws") ["fr"; "krobot"; "Services"; "Claws"]

let enable krobot =
  OBus_method.call m_enable (proxy krobot) ()

let disable krobot =
  OBus_method.call m_disable (proxy krobot) ()

let open_ krobot =
  OBus_method.call m_open (proxy krobot) ()

let close krobot =
  OBus_method.call m_close (proxy krobot) ()

let take krobot =
  OBus_method.call m_take (proxy krobot) ()
