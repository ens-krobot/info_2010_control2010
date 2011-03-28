(*
 * krobot_daemon.mli
 * -----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(** Krobot daemon utilities *)

(** {6 Controlling remote daemons} *)

val log : OBus_peer.t -> string OBus_signal.t
  (** [log daemon] returns the signal receiving logs emited by the
      given daemon *)

val shutdown : OBus_peer.t -> unit Lwt.t
  (** [shutdown daemon] gracefully shutdowns the given daemon *)

(** {6 Running daemons} *)

val start : desc : string -> name : OBus_name.bus -> (OBus_bus.t -> unit Lwt.t) -> unit Lwt.t
  (** [init ~desc ~name init] does common daemon initializations,
      request the given bus name, open the robot bus and pass it to
      [init]. [init] should do specifics initialisations, like
      exporting objects on D-Bus, then return.

      When [name] is lost, the program will be exited with code 0. *)
