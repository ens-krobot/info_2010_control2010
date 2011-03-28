(*
 * error.mli
 * ---------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(** D-Bus Errors that may be reported by krobot applications *)

(** Device errors *)
module Device : sig
  exception Error of string
    (** D-Bus exception raised when a card return an error *)

  exception Closed of string
    (** D-Bus exception raised when trying to use a closed device. *)

  exception Unavailable of string
    (** D-Bus exception raised when trying to use a device that is not
        currently up. *)

  exception Not_implemented of string
    (** D-Bus exception raised when calling a request that is not
        implemented by the device. *)

  exception Timeout of string
    (** D-Bus exception raised when a call timeout *)
end
