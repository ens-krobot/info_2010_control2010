(*
 * krobot_driver.ml
 * ----------------
 * Copyright : (c) 2009-2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* The [kro]bot's driver.

   Its only goal is to publish krobot devices' methods on D-Bus. The
   advantage of getting access to devices through a seperate process
   is that multiple programs can simultaneously access devices. This
   allow to write many smalll programs doing only one thing rather
   than having a big monolitic program doing everything. *)

let section = Lwt_log.Section.make "driver"

open Lwt
open Krobot_wire

(* +-----------------------------------------------------------------+
   | Devices                                                         |
   +-----------------------------------------------------------------+ *)

let device_path name = ["fr"; "krobot"; "Devices"; name]

module Analogic_motor =
struct
  open Krobot_dbus_driver.Fr_krobot_LowLevel_AnalogicMotor

  let set_state card ~state =
    Krobot_usb.call_no_reply card
      ~command:PcInterface.cmd_motor
      ~i_types:(seq2 uint8 uint8)
      ((if state then PcInterface.motor_enable else PcInterface.motor_disable),
       PcInterface.motor_both)

  let set_velocity card ~velocity ~duration =
    Krobot_usb.call_no_reply card
      ~command:PcInterface.cmd_motor
      ~i_types:(seq5 uint8 uint8 boolean uint8 uint32)
      (PcInterface.motor_move,
       PcInterface.motor_both,
       (if velocity < 0 then false else true),
       abs velocity,
       truncate (duration *. 1000.))

  let interface =
    make {
      m_set_state = (
        fun obj state ->
          set_state (OBus_object.get obj) ~state
      );
      m_set_velocity = (
        fun obj (velocity, duration) ->
          let velocity = Int32.to_int velocity in
          set_velocity (OBus_object.get obj) velocity duration
      );
    }

  let make card =
    let obus = OBus_object.make ~interfaces:[interface] (device_path "AnalogicMotor") in
    OBus_object.attach obus card;
    obus
end

module Analogic_servos =
struct
  open Krobot_dbus_driver.Fr_krobot_LowLevel_AnalogicServos

  let set_config card ~enable ~disable =
    Krobot_usb.call_no_reply card
      ~command:PcInterface.cmd_set
      ~i_types:(seq3 uint8 uint8 uint8)
      (PcInterface.set_servo_config,
       List.fold_left (fun acc n -> acc lor (1 lsl n)) 0 enable,
       List.fold_left (fun acc n -> acc lor (1 lsl n)) 0 disable)

  let set_state card ~states =
    let get id mask =
      try
        (List.assoc id states, mask lor (1 lsl id))
      with Not_found ->
        (0, mask)
    in
    let mask = 0 in
    let angle0, mask = get 0 mask in
    let angle1, mask = get 1 mask in
    let angle2, mask = get 2 mask in
    let angle3, mask = get 3 mask in
    let angle4, mask = get 4 mask in
    Krobot_usb.call_no_reply card
      ~command:PcInterface.cmd_set
      ~i_types:(seq3 uint8 uint8 (array 5 int8))
      (PcInterface.set_servo_state,
       mask,
       [|angle0; angle1; angle2; angle3; angle4|])

  let interface =
    make {
      m_set_config = (
        fun obj (enable, disable) ->
          let enable = List.map Int32.to_int enable in
          let disable = List.map Int32.to_int disable in
          set_config (OBus_object.get obj) enable disable
      );
      m_set_state = (
        fun obj states ->
          let states = List.map (fun (k, v) -> (Int32.to_int k, Int32.to_int v)) states in
          set_state (OBus_object.get obj) states
      );
    }

  let make card =
    let obus = OBus_object.make ~interfaces:[interface] (device_path "AnalogicServos") in
    OBus_object.attach obus card;
    obus
end

module Compass =
struct
  open Krobot_dbus_driver.Fr_krobot_LowLevel_Compass

  let get_measure card =
    lwt dummy, value =
      Krobot_usb.call card
        ~command:PcInterface.cmd_get
        ~i_types:uint8
        ~o_types:(seq2 uint16 uint16)
        PcInterface.get_cmp03_data
    in
    return value

  let interface =
    make {
      m_get_measure = (
        fun obj () ->
          get_measure (OBus_object.get obj) >|= Int32.of_int
      );
    }

  let make card =
    let obus = OBus_object.make ~interfaces:[interface] (device_path "Compass") in
    OBus_object.attach obus card;
    obus
end

type dynamixel_register8 =
    [ `Version_of_firmware
    | `Id
    | `Baud_rate
    | `Return_delay_time
    | `Highest_limit_temperature
    | `Lowest_limit_voltage
    | `Highest_limit_voltage
    | `Status_return_level
    | `Alarm_led
    | `Alarm_shutdown
    | `Torque_enable
    | `Led
    | `Cw_compliance_margin
    | `Ccw_compliance_margin
    | `Cw_compliance_slope
    | `Ccw_compliance_slope
    | `Present_voltage
    | `Present_temperature
    | `Registered_instruction
    | `Moving
    | `Lock ]

type dynamixel_register16 =
    [ `Model_number
    | `Cw_angle_limit
    | `Ccw_angle_limit
    | `Max_torque
    | `Down_calibration
    | `Up_calibration
    | `Goal_position
    | `Moving_speed
    | `Torque_limit
    | `Present_position
    | `Present_speed
    | `Present_load
    | `Punch ]

module AX12 =
struct
  open Krobot_dbus_driver.Fr_krobot_LowLevel_Dynamixel

  let timeout = 1000

  let ping card ~id =
    Krobot_usb.call card
      ~command:PcInterface.cmd_ax12
      ~i_types:(seq3 uint8 uint8 uint16)
      ~o_types:unit
      (PcInterface.ax12_ping, id, timeout)

  let action card ~id =
    Krobot_usb.call card
      ~command:PcInterface.cmd_ax12
      ~i_types:(seq3 uint8 uint8 uint16)
      ~o_types:unit
      (PcInterface.ax12_action, id, timeout)

  let reset card ~id =
    Krobot_usb.call_no_reply card
      ~command:PcInterface.cmd_ax12
      ~i_types:(seq2 uint8 uint8)
      (PcInterface.ax12_reset, id)

  let get card ~id ~address =
    match address with
      | #dynamixel_register8 ->
          Krobot_usb.call card
            ~command:PcInterface.cmd_ax12
            ~i_types:(seq4 uint8 uint8 uint8 uint16)
            ~o_types:uint8
            (PcInterface.ax12_read8, id, Int32.to_int (cast_readable_register address), timeout)
      | #dynamixel_register16 ->
          Krobot_usb.call card
            ~command:PcInterface.cmd_ax12
            ~i_types:(seq4 uint8 uint8 uint8 uint16)
            ~o_types:uint16
            (PcInterface.ax12_read16, id, Int32.to_int (cast_readable_register address), timeout)

  let set card ~id ~address ~value =
    match address with
      | #dynamixel_register8 ->
          Krobot_usb.call card
            ~command:PcInterface.cmd_ax12
            ~i_types:(seq4 uint8 uint8 uint8 uint8)
            ~o_types:unit
            (PcInterface.ax12_write8, id, Int32.to_int (cast_writable_register address), value)
      | #dynamixel_register16 ->
          Krobot_usb.call card
            ~command:PcInterface.cmd_ax12
            ~i_types:(seq4 uint8 uint8 uint8 uint16)
            ~o_types:unit
            (PcInterface.ax12_write16, id, Int32.to_int (cast_writable_register address), value)

  let reg_set card ~id ~address ~value =
    match address with
      | #dynamixel_register8 ->
          Krobot_usb.call card
            ~command:PcInterface.cmd_ax12
            ~i_types:(seq4 uint8 uint8 uint8 uint8)
            ~o_types:unit
            (PcInterface.ax12_write_reg8, id, Int32.to_int (cast_writable_register address), value)
      | #dynamixel_register16 ->
          Krobot_usb.call card
            ~command:PcInterface.cmd_ax12
            ~i_types:(seq4 uint8 uint8 uint8 uint16)
            ~o_types:unit
            (PcInterface.ax12_write_reg16, id, Int32.to_int (cast_writable_register address), value)

  let goto card ~id ~position ~speed ~mode =
    Krobot_usb.call card
      ~command:PcInterface.cmd_ax12
      ~i_types:(seq5 uint8 uint8 uint16 uint16 uint8)
      ~o_types:unit
      (PcInterface.ax12_goto, id, position, speed,
       (match mode with
          | `Now -> PcInterface.ax12_exec_now
          | `Action -> PcInterface.ax12_exec_action))

  let config card ~id =
    Krobot_usb.call card
      ~command:PcInterface.cmd_ax12
      ~i_types:(seq2 uint8 uint8)
      ~o_types:unit
      (PcInterface.ax12_config, id)

  let get_position card ~id =
    Krobot_usb.call card
      ~command:PcInterface.cmd_ax12
      ~i_types:(seq3 uint8 uint8 uint16)
      ~o_types:uint16
      (PcInterface.ax12_get_pos, id, timeout)

  let get_speed card ~id =
    Krobot_usb.call card
      ~command:PcInterface.cmd_ax12
      ~i_types:(seq3 uint8 uint8 uint16)
      ~o_types:uint16
      (PcInterface.ax12_get_speed, id, timeout)

  let interface =
    make {
      m_ping = (
        fun obj id ->
          let id = Int32.to_int id in
          ping (OBus_object.get obj) id
      );
      m_action = (
        fun obj id ->
          let id = Int32.to_int id in
          action (OBus_object.get obj) id
      );
      m_reset = (
        fun obj id ->
          let id = Int32.to_int id in
          reset (OBus_object.get obj) id
      );
      m_get = (
        fun obj (id, address) ->
          let id = Int32.to_int id in
          let address = make_readable_register address in
          get (OBus_object.get obj) id address >|= Int32.of_int
      );
      m_set = (
        fun obj (id, address, value) ->
          let id = Int32.to_int id in
          let address = make_writable_register address in
          let value = Int32.to_int value in
          set (OBus_object.get obj) id address value
      );
      m_reg_set = (
        fun obj (id, address, value) ->
          let id = Int32.to_int id in
          let address = make_writable_register address in
          let value = Int32.to_int value in
          reg_set (OBus_object.get obj) id address value
      );
      m_goto = (
        fun obj (id, position, speed, mode) ->
          let id = Int32.to_int id in
          let position = Int32.to_int position in
          let speed = Int32.to_int speed in
          let mode = make_exec_mode mode in
          goto (OBus_object.get obj) id position speed mode
      );
      m_config = (
        fun obj id ->
          let id = Int32.to_int id in
          config (OBus_object.get obj) id
      );
      m_get_position = (
        fun obj id ->
          let id = Int32.to_int id in
          get_position (OBus_object.get obj) id >|= Int32.of_int
      );
      m_get_speed = (
        fun obj id ->
          let id = Int32.to_int id in
          get_speed (OBus_object.get obj) id >|= Int32.of_int
      );
    }

  let make card =
    let obus = OBus_object.make ~interfaces:[interface] (device_path "AX12") in
    OBus_object.attach obus card;
    obus
end

module RX64 =
struct
  open Krobot_dbus_driver.Fr_krobot_LowLevel_Dynamixel

  let ping card ~id =
    Krobot_dynamixel_over_serial.ping card id

  let action card ~id =
    raise_lwt (Failure "not implemented")

  let reset card ~id =
    Krobot_dynamixel_over_serial.reset card id

  let get card ~id ~address =
    match address with
      | #dynamixel_register8 ->
          lwt data = Krobot_dynamixel_over_serial.read card id (Int32.to_int (cast_readable_register address)) 1 in
          return (int_of_char data.[0])
      | #dynamixel_register16 ->
          lwt data = Krobot_dynamixel_over_serial.read card id (Int32.to_int (cast_readable_register address)) 2 in
          return (((int_of_char data.[1]) lsl 8) lor (int_of_char data.[0]))

  let set card ~id ~address ~value =
    match address with
      | #dynamixel_register8 ->
          let data = String.create 1 in
          data.[0] <- char_of_int value;
          Krobot_dynamixel_over_serial.write card id (Int32.to_int (cast_writable_register address)) data
      | #dynamixel_register16 ->
          let data = String.create 2 in
          data.[0] <- char_of_int (value land 0xff);
          data.[1] <- char_of_int ((value lsr 8) land 0xff);
          Krobot_dynamixel_over_serial.write card id (Int32.to_int (cast_writable_register address)) data

  let reg_set card ~id ~address ~value =
    raise_lwt (Failure "not implemented")

  let goto card ~id ~position ~speed ~mode =
    lwt () = set card ~id ~address:`Moving_speed ~value:speed in
    set card ~id ~address:`Goal_position ~value:position

  let config card ~id =
    raise_lwt (Failure "not implemented")

  let get_position card ~id =
    get card ~id ~address:`Present_position

  let get_speed card ~id =
    get card ~id ~address:`Present_speed

  let interface =
    make {
      m_ping = (
        fun obj id ->
          let id = Int32.to_int id in
          ping (OBus_object.get obj) ~id
      );
      m_action = (
        fun obj id ->
          let id = Int32.to_int id in
          action (OBus_object.get obj) ~id
      );
      m_reset = (
        fun obj id ->
          let id = Int32.to_int id in
          reset (OBus_object.get obj) ~id
      );
      m_get = (
        fun obj (id, address) ->
          let id = Int32.to_int id in
          let address = make_readable_register address in
          get (OBus_object.get obj) ~id ~address >|= Int32.of_int
      );
      m_set = (
        fun obj (id, address, value) ->
          let id = Int32.to_int id in
          let address = make_writable_register address in
          let value = Int32.to_int value in
          set (OBus_object.get obj) ~id ~address ~value
      );
      m_reg_set = (
        fun obj (id, address, value) ->
          let id = Int32.to_int id in
          let address = make_writable_register address in
          let value = Int32.to_int value in
          reg_set (OBus_object.get obj) ~id ~address ~value
      );
      m_goto = (
        fun obj (id, position, speed, mode) ->
          let id = Int32.to_int id in
          let position = Int32.to_int position in
          let speed = Int32.to_int speed in
          let mode = make_exec_mode mode in
          goto (OBus_object.get obj) ~id ~position ~speed ~mode
      );
      m_config = (
        fun obj id ->
          let id = Int32.to_int id in
          config (OBus_object.get obj) id
      );
      m_get_position = (
        fun obj id ->
          let id = Int32.to_int id in
          get_position (OBus_object.get obj) ~id >|= Int32.of_int
      );
      m_get_speed = (
        fun obj id ->
          let id = Int32.to_int id in
          get_speed (OBus_object.get obj) ~id >|= Int32.of_int
      );
    }

  let make card =
    let obus = OBus_object.make ~interfaces:[interface] (device_path "RX64") in
    OBus_object.attach obus card;
    obus
end

module Infrareds =
struct
  open Krobot_dbus_driver.Fr_krobot_LowLevel_Infrareds

  let get card =
    Krobot_usb.call card
      ~command:PcInterface.cmd_get
      ~i_types:uint8
      ~o_types:(array 4 uint32)
      PcInterface.get_rangefinder_state

  let interface =
    make {
      m_get = (
        fun obj () ->
          lwt result = get (OBus_object.get obj) in
          return (List.map Int32.of_int (Array.to_list result))
      );
    }

  let make card =
    let obus = OBus_object.make ~interfaces:[interface] (device_path "Infrareds") in
    OBus_object.attach obus card;
    obus
end

module LCD =
struct
  open Krobot_dbus_driver.Fr_krobot_LowLevel_LCD

  let clear card () =
    Krobot_usb.call_no_reply card
      ~command:PcInterface.cmd_lcd
      ~i_types:uint8
      PcInterface.lcd_clear

  let set_cursor card ~state =
    Krobot_usb.call_no_reply card
      ~command:PcInterface.cmd_lcd
      ~i_types:uint8
      (if state then PcInterface.lcd_cursor_on else PcInterface.lcd_cursor_off)

  let set_backlight card ~state =
    Krobot_usb.call_no_reply card
      ~command:PcInterface.cmd_lcd
      ~i_types:uint8
      (if state then PcInterface.lcd_backlight_on else PcInterface.lcd_backlight_off)

  let goto card ~line ~column =
    Krobot_usb.call_no_reply card
      ~command:PcInterface.cmd_lcd
      ~i_types:(seq3 uint8 uint8 uint8)
      (PcInterface.lcd_goto_pos, column, line)

  let write card ~text =
    Krobot_usb.call_no_reply card
      ~command:PcInterface.cmd_lcd
      ~i_types:(seq2 uint8 string)
      (PcInterface.lcd_write, text)

  let write_line card ~line ~text =
    Krobot_usb.call_no_reply card
      ~command:PcInterface.cmd_lcd
      ~i_types:(seq3 uint8 uint8 string)
      (PcInterface.lcd_write_line, line, text)

  let interface =
    make {
      m_clear = (
        fun obj () ->
          clear (OBus_object.get obj) ()
      );
      m_set_cursor = (
        fun obj state ->
          set_cursor (OBus_object.get obj) state
      );
      m_set_backlight = (
        fun obj state ->
          set_backlight (OBus_object.get obj) state
      );
      m_goto = (
        fun obj (line, column) ->
          let line = Int32.to_int line in
          let column = Int32.to_int column in
          goto (OBus_object.get obj) line column
      );
      m_write = (
        fun obj text ->
          write (OBus_object.get obj) text
      );
      m_write_line = (
        fun obj (line, text) ->
          let line = Int32.to_int line in
          write_line (OBus_object.get obj) line text
      );
    }

  let make card =
    let obus = OBus_object.make ~interfaces:[interface] (device_path "LCD") in
    OBus_object.attach obus card;
    obus
end

module Logic_sensors =
struct
  open Krobot_dbus_driver.Fr_krobot_LowLevel_LogicSensors

  let get_state card () =
    lwt bits =
      Krobot_usb.call card
        ~command:PcInterface.cmd_get
        ~i_types:uint8
        ~o_types:uint16
        PcInterface.get_tor_state
    in
    let rec loop n =
      if n = 16 then
        []
      else
        (bits land (1 lsl n) <> 0) :: loop (n + 1)
    in
    return (loop 0)

  let interface =
    make {
      m_get_state = (
        fun obj () ->
          get_state (OBus_object.get obj) ()
      );
    }

  let make card =
    let obus = OBus_object.make ~interfaces:[interface] (device_path "LogicSensors") in
    OBus_object.attach obus card;
    obus
end

module Motors =
struct
  open Krobot_dbus_driver.Fr_krobot_LowLevel_Motors

  let map_dist x = truncate (x *. 1000.0)
  let map_angle x = truncate (x *. 180.0 /. Krobot_geometry.pi)

  let thread = ref (return ())
    (* Thread waiting for movement completion or timeout expiration *)

  (* Movements mode *)
  type mode =
    | Mode_none
        (* Not moving *)
    | Mode_managed
        (* Movements with move/turn *)
    | Mode_manual
        (* Movements with set_velocities *)

  let wait_for_completion card =
    let w =
      lwt _ =
        Lwt_event.next
          (React.E.filter
             (fun (cmd, data) -> cmd = PcInterface.cmd_traj && Char.code data.[0] = PcInterface.traj_completed)
             (Krobot_usb.commands card))
      in
      return ()
    in
    thread := w;
    w

  let init card =
    Krobot_usb.call_no_reply card
      ~command:PcInterface.cmd_traj
      ~i_types:uint8
      PcInterface.traj_init

  let mode = ref Mode_none

  let switch_mode card new_mode =
    if new_mode <> !mode then begin
      mode := new_mode;
      init card
    end else
      return ()

  let move card ~distance ~velocity ~acceleration =
    cancel !thread;
    lwt () = switch_mode card Mode_managed in
    lwt () =
      Krobot_usb.call_no_reply card
        ~command:PcInterface.cmd_traj
        ~i_types:(seq4 uint8 int16 int16 int16)
        ((if distance >= 0.0 then PcInterface.traj_forward else PcInterface.traj_backward),
         abs (map_dist distance), map_dist velocity, map_dist acceleration)
    in
    wait_for_completion card

  let turn card ~angle ~velocity ~acceleration =
    cancel !thread;
    lwt () = switch_mode card Mode_managed in
    lwt () =
      Krobot_usb.call_no_reply card
        ~command:PcInterface.cmd_traj
        ~i_types:(seq4 uint8 int16 int16 int16)
        ((if angle >= 0.0 then PcInterface.traj_tl else PcInterface.traj_tr),
         abs (map_angle angle), map_dist velocity, map_dist acceleration)
    in
    wait_for_completion card

  let code_of_motor = function
    | `Both -> PcInterface.motor_both
    | `Left -> PcInterface.motor_left
    | `Right -> PcInterface.motor_right

  let stop card ~motor ~mode =
    cancel !thread;
    Krobot_usb.call_no_reply card
      ~command:PcInterface.cmd_traj
      ~i_types:(seq3 uint8 uint8 uint8)
      (PcInterface.traj_stop,
       code_of_motor motor,
       (match mode with
          | `Off -> PcInterface.traj_stop_motor_off
          | `Abrupt -> PcInterface.traj_stop_abrupt
          | `Smooth -> PcInterface.traj_stop_smooth))

  let set_velocities card ~velocity_r ~velocity_l ~duration =
    cancel !thread;
    let direction x = if x < 0.0 then -1 else 1 in
    lwt () =
      match !mode with
        | Mode_none | Mode_managed ->
            mode := Mode_manual;
            (* It the first manual command, we reinit the motors and
               use [traj_new_velocity] *)
            lwt () = init card in
            lwt () =
              Krobot_usb.call_no_reply card
                ~command:PcInterface.cmd_traj
                ~i_types:(seq5 uint8 uint8 int16 int16 int8)
                (PcInterface.traj_new_velocity, code_of_motor `Right, abs (map_dist velocity_r), 800, direction velocity_r)
            and () =
              Krobot_usb.call_no_reply card
                ~command:PcInterface.cmd_traj
                ~i_types:(seq5 uint8 uint8 int16 int16 int8)
                (PcInterface.traj_new_velocity, code_of_motor `Left, abs (map_dist velocity_l), 800, direction velocity_l)
            in
            return ()
        | Mode_manual ->
            (* We are already in manual mode, use
               [traj_change_velocity] *)
            lwt () =
              Krobot_usb.call_no_reply card
                ~command:PcInterface.cmd_traj
                ~i_types:(seq4 uint8 uint8 int16 int8)
                (PcInterface.traj_change_velocity, code_of_motor `Right,
                 abs (map_dist velocity_r), direction velocity_r)
            and () =
              Krobot_usb.call_no_reply card
                ~command:PcInterface.cmd_traj
                ~i_types:(seq4 uint8 uint8 int16 int8)
                (PcInterface.traj_change_velocity, code_of_motor `Left,
                 abs (map_dist velocity_l), direction velocity_l)
            in
            return ()
    in
    lwt () =
      Krobot_usb.call_no_reply card
        ~command:PcInterface.cmd_traj
        ~i_types:(seq2 uint8 uint8)
        (PcInterface.traj_start, code_of_motor `Both)
    in
    (* Stop the robot after [duration] seconds *)
    thread := begin
      lwt () = Lwt_unix.sleep duration in
      lwt () =
        Krobot_usb.call_no_reply card
          ~command:PcInterface.cmd_traj
          ~i_types:(seq5 uint8 uint8 int16 int16 int8)
          (PcInterface.traj_new_velocity, code_of_motor `Both, 0, 0, 1)
      in
      Krobot_usb.call_no_reply card
        ~command:PcInterface.cmd_traj
        ~i_types:(seq2 uint8 uint8)
        (PcInterface.traj_start, code_of_motor `Both)
    end;
    return ()

  let get_current_velocities card =
    lwt vr, vl =
      Krobot_usb.call card
        ~command:PcInterface.cmd_get
        ~i_types:uint8
        ~o_types:(seq2 int32 int32)
        PcInterface.get_current_speed
    in
    return (float_of_int vr /. 1000.0,
            float_of_int vl /. 1000.0)

  let get_current_positions card =
    lwt pr, pl =
      Krobot_usb.call card
        ~command:PcInterface.cmd_get
        ~i_types:uint8
        ~o_types:(seq2 int32 int32)
        PcInterface.get_current_pos
    in
    return (float_of_int pr /. 1000.0,
            float_of_int pl /. 1000.0)

  let get_config card =
    Krobot_usb.call card
      ~command:PcInterface.cmd_traj
      ~i_types:uint8
      ~o_types:(seq8 uint16 uint16 uint16 uint16 uint16 uint16 uint16 uint16)
      PcInterface.traj_read_config

  let set_config card ~motor kp ~ki ~kd ~li =
    Krobot_usb.call_no_reply card
      ~command:PcInterface.cmd_traj
      ~i_types:(seq6 uint8 uint8 uint16 uint16 uint16 uint16)
      (PcInterface.traj_config, code_of_motor motor,
       kp, ki, kd, li)

  let interface =
    make {
      m_move = (
        fun obj (distance, velocity, acceleration) ->
          move (OBus_object.get obj) distance velocity acceleration
      );
      m_turn = (
        fun obj (angle, velocity, acceleration) ->
          turn (OBus_object.get obj) angle velocity acceleration
      );
      m_stop = (
        fun obj (motor, mode) ->
          let motor = make_motor motor in
          let mode = make_stop_mode mode in
          stop (OBus_object.get obj) motor mode
      );
      m_set_velocities = (
        fun obj (velocity_r, velocity_l, duration) ->
          set_velocities (OBus_object.get obj) velocity_r velocity_l duration
      );
      m_get_current_velocities = (
        fun obj () ->
          get_current_velocities (OBus_object.get obj)
      );
      m_get_current_positions = (
        fun obj () ->
          get_current_positions (OBus_object.get obj)
      );
      m_get_config = (
        fun obj () ->
          lwt (kp_r, ki_r, kd_r, li_r, kp_l, ki_l, kd_l, li_l) = get_config (OBus_object.get obj) in
          return (Int32.of_int kp_r,
                  Int32.of_int ki_r,
                  Int32.of_int kd_r,
                  Int32.of_int li_r,
                  Int32.of_int kp_l,
                  Int32.of_int ki_l,
                  Int32.of_int kd_l,
                  Int32.of_int li_l)
      );
      m_set_config = (
        fun obj (motor, kp, ki, kd, li) ->
          let motor = make_motor motor in
          let kp = Int32.to_int kp in
          let ki = Int32.to_int ki in
          let kd = Int32.to_int kd in
          let li = Int32.to_int li in
          set_config (OBus_object.get obj) motor kp ki kd li
      );
    }

  let make card =
    let obus = OBus_object.make ~interfaces:[interface] (device_path "Motors") in
    OBus_object.attach obus card;
    obus
end

module Power =
struct
  open Krobot_dbus_driver.Fr_krobot_LowLevel_Power

  type battery_state = type_battery_state

  let set_buzzer_state card ~state =
    Krobot_usb.call_no_reply card
      ~command:PcInterface.cmd_set
      ~i_types:(seq2 uint8 boolean)
      (PcInterface.set_buzzer_state, state)

 let get_cell_voltage card =
    Krobot_usb.call card
      ~command:PcInterface.cmd_get
      ~i_types:uint8
      ~o_types:(array 8 uint16)
      PcInterface.get_cell_voltage

  let get_current card =
    Krobot_usb.call card
      ~command:PcInterface.cmd_get
      ~i_types:uint8
      ~o_types:uint32
      PcInterface.get_current

  let get_power_state card =
    Krobot_usb.call card
      ~command:PcInterface.cmd_get
      ~i_types:uint8
      ~o_types:boolean
      PcInterface.get_power_state

  let get_battery_state card =
    Krobot_usb.call card
      ~command:PcInterface.cmd_get
      ~i_types:uint8
      ~o_types:uint8
      PcInterface.get_battery_state

  let interface =
    make {
      m_set_buzzer_state = (
        fun obj state ->
          set_buzzer_state (OBus_object.get obj) state
      );
      m_get_cell_voltage = (
        fun obj () ->
          lwt result = get_cell_voltage (OBus_object.get obj) in
          return (List.map Int32.of_int (Array.to_list result))
      );
      m_get_current = (
        fun obj () ->
          get_current (OBus_object.get obj) >|= Int32.of_int
      );
      m_get_power_state = (
        fun obj () ->
          get_power_state (OBus_object.get obj)
      );
      m_get_battery_state = (
        fun obj () ->
          get_battery_state (OBus_object.get obj) >|= Int32.of_int
      );
    }

  let make card =
    let obus = OBus_object.make ~interfaces:[interface] (device_path "Power") in
    OBus_object.attach obus card;
    obus
end

module Range_finders =
struct
  open Krobot_dbus_driver.Fr_krobot_LowLevel_RangeFinders

  let measure card ~id =
    Krobot_usb.call card
      ~command:PcInterface.cmd_usrf
      ~i_types:(seq2 uint8 uint8)
      ~o_types:unit
      (PcInterface.usrf_measure, id)

  let get card ~id =
    Krobot_usb.call card
      ~command:PcInterface.cmd_usrf
      ~i_types:(seq2 uint8 uint8)
      ~o_types:uint16
      (PcInterface.usrf_get, id)

  let interface =
    make {
      m_measure = (
        fun obj id ->
          let id = Int32.to_int id in
          measure (OBus_object.get obj) id
      );
      m_get = (
        fun obj id ->
          let id = Int32.to_int id in
          get (OBus_object.get obj) id >|= Int32.of_int
      );
    }

  let make card =
    let obus = OBus_object.make ~interfaces:[interface] (device_path "RangeFinders") in
    OBus_object.attach obus card;
    obus
end

module Card(Device : Krobot_device.S) =
struct
  open Krobot_dbus_driver.Fr_krobot_LowLevel_Card

  let make card =
    let obus =
      OBus_object.make
        ~interfaces:[make {
                       p_name = (fun obj -> React.S.const (Device.name (OBus_object.get obj)));
                       p_state = (fun obj -> React.S.map cast_state (Device.state (OBus_object.get obj)));
                     }]
        ["fr"; "krobot"; "Cards"; Device.name card]
    in
    OBus_object.attach obus card;
    obus
end

module USB_card =
struct
  open Krobot_dbus_driver.Fr_krobot_LowLevel_USBCard

  let get_firmware_build card =
    Krobot_usb.call card
      ~command:PcInterface.cmd_get
      ~i_types:uint8
      ~o_types:string
      PcInterface.get_firmware_build

  let get_board_info card =
    Krobot_usb.call card
      ~command:PcInterface.cmd_get
      ~i_types:uint8
      ~o_types:string
      PcInterface.get_board_info

  let get_ports_state card =
    Krobot_usb.call card
      ~command:PcInterface.cmd_get
      ~i_types:uint8
      ~o_types:(array 5 uint8)
      PcInterface.get_ports_state

  let bootloader card =
    Krobot_usb.call_no_reply card
      ~command:PcInterface.cmd_bootloader
      ~i_types:unit
      ()

  let reset card =
    Krobot_usb.call_no_reply card
      ~command:PcInterface.cmd_reset
      ~i_types:unit
      ()

  let test card =
    Krobot_usb.call_no_reply card
      ~command:PcInterface.cmd_test
      ~i_types:unit
      ()

  let interface =
    make {
      m_get_firmware_build = (
        fun obj () ->
          get_firmware_build (OBus_object.get obj)
      );
      m_get_board_info = (
        fun obj () ->
          get_board_info (OBus_object.get obj)
      );
      m_get_ports_state = (
        fun obj () ->
          lwt result = get_ports_state (OBus_object.get obj) in
          return (List.map Int32.of_int (Array.to_list result))
      );
      m_bootloader = (
        fun obj () ->
          bootloader (OBus_object.get obj)
      );
      m_reset = (
        fun obj () ->
          reset (OBus_object.get obj)
      );
      m_test = (
        fun obj () ->
          test (OBus_object.get obj)
      );
    }
end

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

let init bus =
  (* Open cards *)
  let interface =
    Krobot_usb.make
      ~name:"Interface"
      ~vendor_id:PcInterface.usb_vid
      ~product_id:PcInterface.usb_pid_robot_interface
      ()
  and monitoring =
    Krobot_usb.make
      ~name:"Monitoring"
      ~vendor_id:PcInterface.usb_vid
      ~product_id:PcInterface.usb_pid_battery_monitoring
      ()
  and sensors =
    Krobot_usb.make
      ~name:"Sensors"
      ~vendor_id:PcInterface.usb_vid
      ~product_id:PcInterface.usb_pid_sensor_interface
      ()
  and motors =
    Krobot_usb.make
      ~name:"Motors"
      ~vendor_id:PcInterface.usb_vid
      ~product_id:PcInterface.usb_pid_motor_controller
      ()
  and rx64 =
    Krobot_dynamixel_over_serial.make
      ~name:"RX64"
      ~path:"/dev/ttyS0"
      ~rate:57600
      ()
  in

  (* Closes all devices on exit *)
  Lwt_main.at_exit
    (fun () ->
       join [
         Krobot_usb.close interface;
         Krobot_usb.close monitoring;
         Krobot_usb.close sensors;
         Krobot_usb.close motors;
         Krobot_dynamixel_over_serial.close rx64;
       ]);

  (* Actions when the interface card come up/down *)
  Lwt_signal.always_notify_s
    (function
       | `Up ->
           (* The first command on ax12 always fails, so we trigger it
              now: *)
           lwt _ = AX12.get_position interface ~id:1 in
           return ()
       | `Down ->
           return ())
    (Krobot_usb.state interface);

  (* Actions when the motor card come up/down *)
  Lwt_signal.always_notify_s
    (function
       | `Up ->
           (* Initializes motors *)
           lwt () =
             Krobot_usb.call_no_reply motors
               ~command:PcInterface.cmd_motor
               ~i_types:(seq2 uint8 uint8)
               (PcInterface.motor_enable, Motors.code_of_motor `Both)
           in
           return ()
       | `Down ->
           return ())
    (Krobot_usb.state motors);

  (* Exports objects on the message bus *)
  OBus_object.export bus (Analogic_motor.make interface);
  OBus_object.export bus (Analogic_servos.make interface);
  OBus_object.export bus (Compass.make interface);
  OBus_object.export bus (AX12.make interface);
  OBus_object.export bus (LCD.make interface);
  OBus_object.export bus (Infrareds.make interface);
  OBus_object.export bus (Motors.make motors);
  OBus_object.export bus (Power.make monitoring);
  OBus_object.export bus (Logic_sensors.make sensors);
  OBus_object.export bus (Range_finders.make sensors);
  OBus_object.export bus (RX64.make rx64);

  let module Card_usb = Card(Krobot_usb) in
  let module Card_dynamixel_over_serial = Card(Krobot_dynamixel_over_serial) in
  let card_interface = Card_usb.make interface in
  let card_sensors = Card_usb.make sensors in
  let card_motors = Card_usb.make motors in
  let card_monitoring = Card_usb.make monitoring in
  let card_rx64 = Card_dynamixel_over_serial.make rx64 in

  OBus_object.add_interfaces card_interface [USB_card.interface];
  OBus_object.add_interfaces card_sensors [USB_card.interface];
  OBus_object.add_interfaces card_motors [USB_card.interface];
  OBus_object.add_interfaces card_monitoring [USB_card.interface];

  OBus_object.export bus card_interface;
  OBus_object.export bus card_sensors;
  OBus_object.export bus card_motors;
  OBus_object.export bus card_monitoring;
  OBus_object.export bus card_rx64;

  Lwt_log.notice ~section "ready, waiting for requests"

lwt () = Krobot_daemon.start ~desc:"krobot driver" ~name:"fr.krobot.Driver" init
