(*
 * krobot_grip.mli
 * ---------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

val up : Krobot.t -> bool Lwt.t
val down : Krobot.t -> unit Lwt.t
val open_ : Krobot.t -> unit Lwt.t
val close : Krobot.t -> unit Lwt.t
val release : Krobot.t -> unit Lwt.t
