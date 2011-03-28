(*
 * krobot_sensors.ml
 * -----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt

open Krobot_dbus_sensors.Fr_krobot_Service_Sensors

let proxy krobot = OBus_proxy.make (OBus_peer.make (Krobot.to_bus krobot) "fr.krobot.Service.Sensors") ["fr"; "krobot"; "Services"; "Sensors"]

type color = type_color

let color krobot =
  OBus_property.map_r
    make_color
    (OBus_property.make p_color (proxy krobot))

let infrareds krobot =
  OBus_property.map_r
    (fun x -> Array.of_list (List.map Int32.to_int x))
    (OBus_property.make p_infrareds (proxy krobot))

let logic_sensors krobot =
  OBus_property.map_r
    Array.of_list
    (OBus_property.make p_logic_sensors (proxy krobot))

let range_finders krobot =
  OBus_property.map_r
    (fun x -> Array.of_list (List.map Int32.to_int x))
    (OBus_property.make p_range_finders (proxy krobot))
