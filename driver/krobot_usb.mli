(*
 * krobot_usb.mli
 * --------------
 * Copyright : (c) 2009-2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(** USB card devices *)

include Krobot_device.S

val make : name : string -> vendor_id : int -> product_id : int -> unit -> t
  (** [make ?timeout ~name ~vendor_id ~product_id ()] creates a device
      for a USB card of the robot. *)

val call : t ->
  ?timeout : float ->
  command : int ->
  i_types : 'a Krobot_wire.convertor ->
  o_types : 'b Krobot_wire.convertor ->
  'a -> 'b Lwt.t
  (** [call dev ~command ~i_types ~o_types args] calls a method of the
      card. If the reply does not come before [timeout], {!call} fails
      with [Krobot_error.Timeout]. *)

val call_no_reply : t ->
  command : int ->
  i_types : 'a Krobot_wire.convertor ->
  'a -> unit Lwt.t
  (** [call ~command ~i_types ~o_types args] calls a method of the
      card, but do not wait for a reply *)

val commands : t -> (int * string) React.event
  (** Event which occurs each time a command is received from the
      card *)

val errors : t -> string React.event
  (** Event which occurs each time the card send an error *)
