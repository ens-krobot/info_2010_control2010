(*
 * krobot_driver.mli
 * -----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(** Interface to the krobot driver *)

(** For documentation look at the file "info/control/protocol/krobot_dbus_driver.obus" *)

(** {6 Devices} *)

type 'a device = private OBus_proxy.t

val analogic_motor : Krobot.t -> [ `Analogic_motor ] device
val analogic_servos : Krobot.t -> [ `Analogic_servos ] device
val compass : Krobot.t -> [ `Compass ] device
val ax12 : Krobot.t -> [ `Dynamixel ] device
val rx64 : Krobot.t -> [ `Dynamixel ] device
val infrareds : Krobot.t -> [ `Infrareds ] device
val lcd : Krobot.t -> [ `LCD ] device
val logic_sensors : Krobot.t -> [ `Logic_sensors ] device
val range_finders : Krobot.t -> [ `Range_finders ] device
val motors : Krobot.t -> [ `Motors ] device
val power : Krobot.t -> [ `Power ] device

val card_interface : Krobot.t -> [ `Card | `USB_card ] device
val card_sensors : Krobot.t -> [ `Card | `USB_card ] device
val card_motors : Krobot.t -> [ `Card | `USB_card ] device
val card_monitoring : Krobot.t -> [ `Card | `USB_card ] device
val card_rx64 : Krobot.t -> [ `Card ] device

(** {6 Devices interfaces} *)

module Analogic_motor : sig
  val set_state : [> `Analogic_motor ] device -> state : bool -> unit Lwt.t
  val set_velocity : [> `Analogic_motor ] device -> velocity : int -> duration : float -> unit Lwt.t
end

module Analogic_servos : sig
  val set_config : [> `Analogic_servos ] device -> enable : int list -> disable : int list -> unit Lwt.t
  val set_state : [> `Analogic_servos ] device -> states : (int * int) list -> unit Lwt.t
end

module Card : sig
  type state =
    [ `Up
    | `Down ]
  val name : [> `Card ] device -> (string, [ `readable ]) OBus_property.t
  val state : [> `Card ] device -> (state, [ `readable ]) OBus_property.t
end

module Compass : sig
  val get_measure : [> `Compass ] device -> int Lwt.t
end

module Dynamixel : sig
  type readable_register =
    [ `Model_number
    | `Version_of_firmware
    | `Id
    | `Baud_rate
    | `Return_delay_time
    | `Cw_angle_limit
    | `Ccw_angle_limit
    | `Highest_limit_temperature
    | `Lowest_limit_voltage
    | `Highest_limit_voltage
    | `Max_torque
    | `Status_return_level
    | `Alarm_led
    | `Alarm_shutdown
    | `Down_calibration
    | `Up_calibration
    | `Torque_enable
    | `Led
    | `Cw_compliance_margin
    | `Ccw_compliance_margin
    | `Cw_compliance_slope
    | `Ccw_compliance_slope
    | `Goal_position
    | `Moving_speed
    | `Torque_limit
    | `Present_position
    | `Present_speed
    | `Present_load
    | `Present_voltage
    | `Present_temperature
    | `Registered_instruction
    | `Moving
    | `Lock
    | `Punch ]
  type writable_register =
    [ `Id
    | `Baud_rate
    | `Return_delay_time
    | `Cw_angle_limit
    | `Ccw_angle_limit
    | `Highest_limit_temperature
    | `Lowest_limit_voltage
    | `Highest_limit_voltage
    | `Max_torque
    | `Status_return_level
    | `Alarm_led
    | `Alarm_shutdown
    | `Torque_enable
    | `Led
    | `Cw_compliance_margin
    | `Ccw_compliance_margin
    | `Cw_compliance_slope
    | `Ccw_compliance_slope
    | `Goal_position
    | `Moving_speed
    | `Torque_limit
    | `Registered_instruction
    | `Lock
    | `Punch ]
  type exec_mode =
    [ `Now
    | `Action ]
  val ping : [> `Dynamixel ] device -> id : int -> unit Lwt.t
  val action : [> `Dynamixel ] device -> id : int -> unit Lwt.t
  val reset : [> `Dynamixel ] device -> id : int -> unit Lwt.t
  val get : [> `Dynamixel ] device -> id : int -> address : readable_register -> int Lwt.t
  val set : [> `Dynamixel ] device -> id : int -> address : writable_register -> value : int -> unit Lwt.t
  val reg_set : [> `Dynamixel ] device -> id : int -> address : writable_register -> value : int -> unit Lwt.t
  val goto : [> `Dynamixel ] device -> id : int -> position : int -> speed : int -> mode : exec_mode -> unit Lwt.t
  val config : [> `Dynamixel ] device -> id : int -> unit Lwt.t
  val get_position : [> `Dynamixel ] device -> id : int -> int Lwt.t
  val get_speed : [> `Dynamixel ] device -> id : int -> int Lwt.t
end

module Infrareds : sig
  val get : [> `Infrareds ] device -> int list Lwt.t
end

module LCD : sig
  val clear : [> `LCD ] device -> unit Lwt.t
  val set_cursor : [> `LCD ] device -> state : bool -> unit Lwt.t
  val set_backlight : [> `LCD ] device -> state : bool -> unit Lwt.t
  val goto : [> `LCD ] device -> line : int -> column : int -> unit Lwt.t
  val write : [> `LCD ] device -> text : string -> unit Lwt.t
  val write_line : [> `LCD ] device -> line : int -> text : string -> unit Lwt.t
end

module Logic_sensors : sig
  val get_state : [> `Logic_sensors ] device -> bool list Lwt.t
end

module Motors : sig
  type motor =
    [ `Left
    | `Both
    | `Right ]
  type stop_mode =
    [ `Off
    | `Abrupt
    | `Smooth ]
  val move : [> `Motors ] device -> distance : float -> velocity : float -> acceleration : float -> unit Lwt.t
  val turn : [> `Motors ] device -> angle : float -> velocity : float -> acceleration : float -> unit Lwt.t
  val stop : [> `Motors ] device -> motor : motor -> mode : stop_mode -> unit Lwt.t
  val set_velocities : [> `Motors ] device -> velocity_r : float -> velocity_l : float -> duration : float -> unit Lwt.t
  val get_current_velocities : [> `Motors ] device -> (float * float) Lwt.t
  val get_current_positions : [> `Motors ] device -> (float * float) Lwt.t
  val get_config : [> `Motors ] device -> (int * int * int * int * int * int * int * int) Lwt.t
  val set_config : [> `Motors ] device -> motor : motor -> kp : int -> ki : int -> kd : int -> li : int -> unit Lwt.t
end

module Power : sig
  type battery_state =
    [ `Disconnected
    | `Low
    | `Middle
    | `Full ]
  val set_buzzer_state : [> `Power ] device -> state : bool -> unit Lwt.t
  val get_cell_voltage : [> `Power ] device -> int list Lwt.t
  val get_current : [> `Power ] device -> int Lwt.t
  val get_power_state : [> `Power ] device -> bool Lwt.t
  val get_battery_state : [> `Power ] device -> battery_state Lwt.t
end

module Range_finders : sig
  val measure : [> `Range_finders ] device -> id : int -> unit Lwt.t
  val get : [> `Range_finders ] device -> id : int -> int Lwt.t
end

module USB_card : sig
  val get_firmware_build : [> `USB_card ] device -> string Lwt.t
  val get_board_info : [> `USB_card ] device -> string Lwt.t
  val get_ports_state : [> `USB_card ] device -> int list Lwt.t
  val bootloader : [> `USB_card ] device -> unit Lwt.t
  val reset : [> `USB_card ] device -> unit Lwt.t
  val test : [> `USB_card ] device -> unit Lwt.t
end
