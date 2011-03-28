(*
 * krobot_dynamixel_over_serial.mli
 * --------------------------------
 * Copyright : (c) 2010, Stéphane Glondu <steph@glondu.net>
 *             (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(** Access to dynamixel servos by serial port *)

include Krobot_device.S

type error =
  | E_Instruction
      (** an undefined instruction is sent or an action instruction is
          sent without a I_REG_WRITE instruction *)
  | E_Overload
      (** the specified maximum torque can't control the applied
          load *)
  | E_Checksum
      (** the checksum of the instruction packaget in incorrect *)
  | E_Range
      (** instruction sent is out of the defined range *)
  | E_Overheating
      (** the internal temperature of the Dynamixel unit is above the
          operating temperating range as defined in the control
          table *)
  | E_AngleLimit
      (** the Goal Position is set outside of the range between CW
          Angle Limit and CCW Angle Limit *)
  | E_InputVoltage
      (** the voltage is out of the operating voltage range as defined
          in the control table *)

val make : ?timeout : float -> name : string -> path : string -> rate : int -> unit -> t
  (** [make ?timeout ~name ~path ~rate ()] creates a device for an
      RX64 controlled over the serial port *)

val ping : t -> id : int -> unit Lwt.t
val read : t -> id : int -> address : int -> length : int -> string Lwt.t
val write : t -> id : int -> address : int -> data : string -> unit Lwt.t
val reset : t -> id : int -> unit Lwt.t
