(*
 * krobot_device.mli
 * -----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(** Abstract devices *)

(** A device is a component of the robot. The following signature
    abstract the communication methods used between the real device
    and the motherboard of the robot. *)

(** Signature of a device of the robot *)
module type S = sig
  type t
    (** Real type for a device *)

  val name : t -> string
    (** Name of the device. It is used for D-Bus integration and for
        logs and error messages. *)

  val state : t -> [ `Up | `Down ] React.signal
    (** Signal holding the current statte of the device. [`Up] means
        that it is available on the system and is ready to process
        requests. *)

  val close : t -> unit Lwt.t
    (** Gracefully close the device. *)
end
