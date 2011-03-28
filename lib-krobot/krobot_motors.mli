(*
 * krobot_motors.mli
 * -----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

type move_result =
    [ `Success
    | `Stopped
    | `Inhibited
    | `Replaced ]
type stop_mode =
    [ `Off
    | `Abrupt
    | `Smooth ]
val turn : Krobot.t -> angle : float -> velocity : float -> acceleration : float -> (move_result * float) Lwt.t
val move : Krobot.t -> distance : float -> velocity : float -> acceleration : float -> (move_result * float) Lwt.t
val stop : Krobot.t -> mode : stop_mode -> unit Lwt.t
val set_velocities : Krobot.t -> velocity_r : float -> velocity_l : float -> duration : float -> unit Lwt.t
val inhibited_forward : Krobot.t -> (bool, [ `readable ]) OBus_property.t
val inhibited_backward : Krobot.t -> (bool, [ `readable ]) OBus_property.t
val inhibit_forward : Krobot.t -> duration : float -> unit Lwt.t
val inhibit_backward : Krobot.t -> duration : float -> unit Lwt.t
