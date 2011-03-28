(*
 * krobot.mli
 * ----------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(** Krobot object *)

type t = private OBus_bus.t
    (** Type of krobot objects *)

external to_bus : t -> OBus_bus.t = "%identity"
external of_bus : OBus_bus.t -> t = "%identity"

val create : unit -> t Lwt.t
  (** [create ()] returns the krobot object. Multiple calls to
      {!create} will returns the same value. *)
