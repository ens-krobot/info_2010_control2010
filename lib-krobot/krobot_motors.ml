(*
 * krobot_motors.ml
 * ----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt

open Krobot_dbus_motors.Fr_krobot_Service_Motors

let proxy krobot = OBus_proxy.make (OBus_peer.make (Krobot.to_bus krobot) "fr.krobot.Service.Motors") ["fr"; "krobot"; "Services"; "Motors"]

type move_result = type_move_result
type stop_mode = type_stop_mode

let turn krobot ~angle ~velocity ~acceleration =
  lwt (result, accomplished) = OBus_method.call m_turn (proxy krobot) (angle, velocity, acceleration) in
  let result = make_move_result result in
  return (result, accomplished)

let move krobot ~distance ~velocity ~acceleration =
  lwt (result, accomplished) = OBus_method.call m_move (proxy krobot) (distance, velocity, acceleration) in
  let result = make_move_result result in
  return (result, accomplished)

let stop krobot ~mode =
  let mode = cast_stop_mode mode in
  OBus_method.call m_stop (proxy krobot) mode

let set_velocities krobot ~velocity_r ~velocity_l ~duration =
  OBus_method.call m_set_velocities (proxy krobot) (velocity_r, velocity_l, duration)

let inhibited_forward krobot =
  OBus_property.make p_inhibited_forward (proxy krobot)

let inhibited_backward krobot =
  OBus_property.make p_inhibited_backward (proxy krobot)

let inhibit_forward krobot ~duration =
  OBus_method.call m_inhibit_forward (proxy krobot) duration

let inhibit_backward krobot ~duration =
  OBus_method.call m_inhibit_backward (proxy krobot) duration
