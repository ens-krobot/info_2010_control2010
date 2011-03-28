(*
 * krobot_gate.mli
 * ---------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

val enable : Krobot.t -> unit Lwt.t
val disable : Krobot.t -> unit Lwt.t
val close : Krobot.t -> unit Lwt.t
val open_ : Krobot.t -> unit Lwt.t
val hold_closed : Krobot.t -> unit Lwt.t
val stop : Krobot.t -> unit Lwt.t
