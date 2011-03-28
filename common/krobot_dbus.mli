(*
 * krobot_dbus.mli
 * ---------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(** D-Bus utilities *)

val open_bus : unit -> OBus_bus.t Lwt.t
  (** Open the message bus used for the robot. The connection to the
      message bus can be given according using either a command or an
      address. A command can be something like:

        ["ssh wally krobot-forward-dbus"]

      This module will looks in order at:
      - the command line argument "-dbus-command"
      - the command line argument "-dbus-address"
      - the environment variable "DBUS_KROBOT_BUS_COMMAND"
      - the environment variable "DBUS_KROBOT_BUS_ADDRESS"

      If no configuration is found, the default address
      ["unix:abstract=krobot"] is used.

      Note: multiple call to [open_bus] will returns the same bus. *)
