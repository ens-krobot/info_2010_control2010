(*
 * krobot_turret.mli
 * -----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

val goto : Krobot.t -> angle : float -> unit Lwt.t
val scan : Krobot.t -> (float * float) list Lwt.t
val find : Krobot.t -> (float * float) Lwt.t
