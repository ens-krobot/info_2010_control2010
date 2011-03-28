(*
 * krobot_driver.ml
 * ----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt
open Krobot_dbus_driver

type 'a device = OBus_proxy.t

let device krobot name =
  OBus_proxy.make (OBus_peer.make (Krobot.to_bus krobot) "fr.krobot.Driver") ["fr"; "krobot"; "Devices"; name]

let analogic_motor krobot = device krobot "AnalogicMotor"
let analogic_servos krobot = device krobot "AnalogicServos"
let compass krobot = device krobot "Compass"
let ax12 krobot = device krobot "AX12"
let rx64 krobot = device krobot "RX64"
let infrareds krobot = device krobot "Infrareds"
let lcd krobot = device krobot "LCD"
let logic_sensors krobot = device krobot "LogicSensors"
let range_finders krobot = device krobot "RangeFinders"
let motors krobot = device krobot "Motors"
let power krobot = device krobot "Power"

let card krobot name =
  OBus_proxy.make (OBus_peer.make (Krobot.to_bus krobot) "fr.krobot.Driver") ["fr"; "krobot"; "Cards"; name]

let card_interface krobot = card krobot "Interface"
let card_sensors krobot = card krobot "Sensors"
let card_motors krobot = card krobot "Motors"
let card_monitoring krobot = card krobot "Monitoring"
let card_rx64 krobot = card krobot "RX64"

module Analogic_motor =
struct
  open Fr_krobot_LowLevel_AnalogicMotor

  let set_state proxy ~state =
    OBus_method.call m_set_state proxy state

  let set_velocity proxy ~velocity ~duration =
    let velocity = Int32.of_int velocity in
    OBus_method.call m_set_velocity proxy (velocity, duration)
end

module Analogic_servos =
struct
  open Fr_krobot_LowLevel_AnalogicServos

  let set_config proxy ~enable ~disable =
    let enable = List.map Int32.of_int enable in
    let disable = List.map Int32.of_int disable in
    OBus_method.call m_set_config proxy (enable, disable)

  let set_state proxy ~states =
    let states = List.map (fun (k, v) -> (Int32.of_int k, Int32.of_int v)) states in
    OBus_method.call m_set_state proxy states
end

module Card =
struct
  open Fr_krobot_LowLevel_Card

  type state = type_state

  let name proxy =
    OBus_property.make p_name proxy

  let state proxy =
    OBus_property.map_r
      (fun x -> make_state x)
      (OBus_property.make p_state proxy)
end

module Compass =
struct
  open Fr_krobot_LowLevel_Compass

  let get_measure proxy =
    lwt value = OBus_method.call m_get_measure proxy () in
    let value = Int32.to_int value in
    return value
end

module Dynamixel =
struct
  open Fr_krobot_LowLevel_Dynamixel

  type readable_register = type_readable_register
  type writable_register = type_writable_register
  type exec_mode = type_exec_mode

  let ping proxy ~id =
    let id = Int32.of_int id in
    OBus_method.call m_ping proxy id

  let action proxy ~id =
    let id = Int32.of_int id in
    OBus_method.call m_action proxy id

  let reset proxy ~id =
    let id = Int32.of_int id in
    OBus_method.call m_reset proxy id

  let get proxy ~id ~address =
    let id = Int32.of_int id in
    let address = cast_readable_register address in
    lwt value = OBus_method.call m_get proxy (id, address) in
    let value = Int32.to_int value in
    return value

  let set proxy ~id ~address ~value =
    let id = Int32.of_int id in
    let address = cast_writable_register address in
    let value = Int32.of_int value in
    OBus_method.call m_set proxy (id, address, value)

  let reg_set proxy ~id ~address ~value =
    let id = Int32.of_int id in
    let address = cast_writable_register address in
    let value = Int32.of_int value in
    OBus_method.call m_reg_set proxy (id, address, value)

  let goto proxy ~id ~position ~speed ~mode =
    let id = Int32.of_int id in
    let position = Int32.of_int position in
    let speed = Int32.of_int speed in
    let mode = cast_exec_mode mode in
    OBus_method.call m_goto proxy (id, position, speed, mode)

  let config proxy ~id =
    let id = Int32.of_int id in
    OBus_method.call m_config proxy id

  let get_position proxy ~id =
    let id = Int32.of_int id in
    lwt position = OBus_method.call m_get_position proxy id in
    let position = Int32.to_int position in
    return position

  let get_speed proxy ~id =
    let id = Int32.of_int id in
    lwt speed = OBus_method.call m_get_speed proxy id in
    let speed = Int32.to_int speed in
    return speed
end

module Infrareds =
struct
  open Fr_krobot_LowLevel_Infrareds

  let get proxy =
    lwt states = OBus_method.call m_get proxy () in
    let states = List.map Int32.to_int states in
    return states
end

module LCD =
struct
  open Fr_krobot_LowLevel_LCD

  let clear proxy =
    OBus_method.call m_clear proxy ()

  let set_cursor proxy ~state =
    OBus_method.call m_set_cursor proxy state

  let set_backlight proxy ~state =
    OBus_method.call m_set_backlight proxy state

  let goto proxy ~line ~column =
    let line = Int32.of_int line in
    let column = Int32.of_int column in
    OBus_method.call m_goto proxy (line, column)

  let write proxy ~text =
    OBus_method.call m_write proxy text

  let write_line proxy ~line ~text =
    let line = Int32.of_int line in
    OBus_method.call m_write_line proxy (line, text)
end

module Logic_sensors =
struct
  open Fr_krobot_LowLevel_LogicSensors

  let get_state proxy =
    OBus_method.call m_get_state proxy ()
end

module Motors =
struct
  open Fr_krobot_LowLevel_Motors

  type motor = type_motor
  type stop_mode = type_stop_mode

  let move proxy ~distance ~velocity ~acceleration =
    OBus_method.call m_move proxy (distance, velocity, acceleration)

  let turn proxy ~angle ~velocity ~acceleration =
    OBus_method.call m_turn proxy (angle, velocity, acceleration)

  let stop proxy ~motor ~mode =
    let motor = cast_motor motor in
    let mode = cast_stop_mode mode in
    OBus_method.call m_stop proxy (motor, mode)

  let set_velocities proxy ~velocity_r ~velocity_l ~duration =
    OBus_method.call m_set_velocities proxy (velocity_r, velocity_l, duration)

  let get_current_velocities proxy =
    OBus_method.call m_get_current_velocities proxy ()

  let get_current_positions proxy =
    OBus_method.call m_get_current_positions proxy ()

  let get_config proxy =
    lwt (kp_r, ki_r, kd_r, li_r, kp_l, ki_l, kd_l, li_l) = OBus_method.call m_get_config proxy () in
    let kp_r = Int32.to_int kp_r in
    let ki_r = Int32.to_int ki_r in
    let kd_r = Int32.to_int kd_r in
    let li_r = Int32.to_int li_r in
    let kp_l = Int32.to_int kp_l in
    let ki_l = Int32.to_int ki_l in
    let kd_l = Int32.to_int kd_l in
    let li_l = Int32.to_int li_l in
    return (kp_r, ki_r, kd_r, li_r, kp_l, ki_l, kd_l, li_l)

  let set_config proxy ~motor ~kp ~ki ~kd ~li =
    let motor = cast_motor motor in
    let kp = Int32.of_int kp in
    let ki = Int32.of_int ki in
    let kd = Int32.of_int kd in
    let li = Int32.of_int li in
    OBus_method.call m_set_config proxy (motor, kp, ki, kd, li)
end

module Power =
struct
  open Fr_krobot_LowLevel_Power

  type battery_state = type_battery_state

  let set_buzzer_state proxy ~state =
    OBus_method.call m_set_buzzer_state proxy state

  let get_cell_voltage proxy =
    lwt states = OBus_method.call m_get_cell_voltage proxy () in
    let states = List.map Int32.to_int states in
    return states

  let get_current proxy =
    lwt current = OBus_method.call m_get_current proxy () in
    let current = Int32.to_int current in
    return current

  let get_power_state proxy =
    OBus_method.call m_get_power_state proxy ()

  let get_battery_state proxy =
    lwt state = OBus_method.call m_get_battery_state proxy () in
    let state = make_battery_state state in
    return state
end

module Range_finders =
struct
  open Fr_krobot_LowLevel_RangeFinders

  let measure proxy ~id =
    let id = Int32.of_int id in
    OBus_method.call m_measure proxy id

  let get proxy ~id =
    let id = Int32.of_int id in
    lwt value = OBus_method.call m_get proxy id in
    let value = Int32.to_int value in
    return value
end

module USB_card =
struct
  open Fr_krobot_LowLevel_USBCard

  let get_firmware_build proxy =
    OBus_method.call m_get_firmware_build proxy ()

  let get_board_info proxy =
    OBus_method.call m_get_board_info proxy ()

  let get_ports_state proxy =
    lwt states = OBus_method.call m_get_ports_state proxy () in
    let states = List.map Int32.to_int states in
    return states

  let bootloader proxy =
    OBus_method.call m_bootloader proxy ()

  let reset proxy =
    OBus_method.call m_reset proxy ()

  let test proxy =
    OBus_method.call m_test proxy ()
end
