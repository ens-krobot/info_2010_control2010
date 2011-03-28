(*
 * krobot_simulator.ml
 * -------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Copyright : (c) 2010, Xavier Lagorce <Xavier.Lagorce@crans.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* The [kro]bot simulator.

   This program replace the driver by simulating the robot. *)

let section = Lwt_log.Section.make "simulator"

open Lwt

(* +-----------------------------------------------------------------+
   | Devices                                                         |
   +-----------------------------------------------------------------+ *)

let device_path name = ["fr"; "krobot"; "Devices"; name]

module Analogic_motor =
struct
  open Krobot_dbus_driver.Fr_krobot_LowLevel_AnalogicMotor

  let set_state obj ~state =
    raise_lwt (Failure "not implemented")

  let set_velocity obj ~velocity ~duration =
    raise_lwt (Failure "not implemented")

  let interface =
    make {
      m_set_state = (
        fun obj state ->
          lwt () = set_state (OBus_object.get obj) state in
          return ()
      );
      m_set_velocity = (
        fun obj (velocity, duration) ->
          let velocity = Int32.to_int velocity in
          lwt () = set_velocity (OBus_object.get obj) velocity duration in
          return ()
      );
    }

  let make () =
    let obus = OBus_object.make ~interfaces:[interface] (device_path "AnalogicMotor") in
    OBus_object.attach obus ();
    obus
end

module Analogic_servos =
struct
  open Krobot_dbus_driver.Fr_krobot_LowLevel_AnalogicServos

  let set_config obj ~enable ~disable =
    raise_lwt (Failure "not implemented")

  let set_state obj ~states =
    raise_lwt (Failure "not implemented")

  let interface =
    make {
      m_set_config = (
        fun obj (enable, disable) ->
          let enable = List.map Int32.to_int enable in
          let disable = List.map Int32.to_int disable in
          lwt () = set_config (OBus_object.get obj) enable disable in
          return ()
      );
      m_set_state = (
        fun obj states ->
          let states = List.map (fun (k, v) -> (Int32.to_int k, Int32.to_int v)) states in
          lwt () = set_state (OBus_object.get obj) states in
          return ()
      );
    }

  let make () =
    let obus = OBus_object.make ~interfaces:[interface] (device_path "AnalogicServos") in
    OBus_object.attach obus ();
    obus
end

module Card =
struct
  open Krobot_dbus_driver.Fr_krobot_LowLevel_Card

  let make name =
    let obus =
      OBus_object.make
        ~interfaces:[make {
                       p_name = (fun obj -> React.S.const name);
                       p_state = (fun obj -> React.S.const (cast_state `Up));
                     }]
        ["fr"; "krobot"; "Cards"; name]
    in
    OBus_object.attach obus ();
    obus
end

module Compass =
struct
  open Krobot_dbus_driver.Fr_krobot_LowLevel_Compass

  let get_measure env =
    return (truncate ((Krobot_env.get_state env).Krobot_env.theta *. 180. /. (4. *. atan 1.)))

  let interface =
    make {
      m_get_measure = (
        fun obj () ->
          lwt value = get_measure (OBus_object.get obj) in
          let value = Int32.of_int value in
          return value
      );
    }

  let make env =
    let obus = OBus_object.make ~interfaces:[interface] (device_path "Compass") in
    OBus_object.attach obus env;
    obus
end

module AX12 =
struct
  open Krobot_dbus_driver.Fr_krobot_LowLevel_Dynamixel

  type readable_register = type_readable_register
  type writable_register = type_writable_register
  type exec_mode = type_exec_mode

  let ping obj ~id =
    raise_lwt (Failure "not implemented")

  let action obj ~id =
    raise_lwt (Failure "not implemented")

  let reset obj ~id =
    raise_lwt (Failure "not implemented")

  let get obj ~id ~address =
    raise_lwt (Failure "not implemented")

  let set obj ~id ~address ~value =
    raise_lwt (Failure "not implemented")

  let reg_set obj ~id ~address ~value =
    raise_lwt (Failure "not implemented")

  let goto obj ~id ~position ~speed ~mode =
    raise_lwt (Failure "not implemented")

  let config obj ~id =
    raise_lwt (Failure "not implemented")

  let get_position obj ~id =
    raise_lwt (Failure "not implemented")

  let get_speed obj ~id =
    raise_lwt (Failure "not implemented")

  let interface =
    make {
      m_ping = (
        fun obj id ->
          let id = Int32.to_int id in
          lwt () = ping (OBus_object.get obj) id in
          return ()
      );
      m_action = (
        fun obj id ->
          let id = Int32.to_int id in
          lwt () = action (OBus_object.get obj) id in
          return ()
      );
      m_reset = (
        fun obj id ->
          let id = Int32.to_int id in
          lwt () = reset (OBus_object.get obj) id in
          return ()
      );
      m_get = (
        fun obj (id, address) ->
          let id = Int32.to_int id in
          let address = make_readable_register address in
          lwt value = get (OBus_object.get obj) id address in
          let value = Int32.of_int value in
          return value
      );
      m_set = (
        fun obj (id, address, value) ->
          let id = Int32.to_int id in
          let address = make_writable_register address in
          let value = Int32.to_int value in
          lwt () = set (OBus_object.get obj) id address value in
          return ()
      );
      m_reg_set = (
        fun obj (id, address, value) ->
          let id = Int32.to_int id in
          let address = make_writable_register address in
          let value = Int32.to_int value in
          lwt () = reg_set (OBus_object.get obj) id address value in
          return ()
      );
      m_goto = (
        fun obj (id, position, speed, mode) ->
          let id = Int32.to_int id in
          let position = Int32.to_int position in
          let speed = Int32.to_int speed in
          let mode = make_exec_mode mode in
          lwt () = goto (OBus_object.get obj) id position speed mode in
          return ()
      );
      m_config = (
        fun obj id ->
          let id = Int32.to_int id in
          lwt () = config (OBus_object.get obj) id in
          return ()
      );
      m_get_position = (
        fun obj id ->
          let id = Int32.to_int id in
          lwt position = get_position (OBus_object.get obj) id in
          let position = Int32.of_int position in
          return position
      );
      m_get_speed = (
        fun obj id ->
          let id = Int32.to_int id in
          lwt speed = get_speed (OBus_object.get obj) id in
          let speed = Int32.of_int speed in
          return speed
      );
    }

  let make () =
    let obus = OBus_object.make ~interfaces:[interface] (device_path "AX12") in
    OBus_object.attach obus ();
    obus
end

module RX64 =
struct
  open Krobot_dbus_driver.Fr_krobot_LowLevel_Dynamixel

  type readable_register = type_readable_register
  type writable_register = type_writable_register
  type exec_mode = type_exec_mode

  let ping obj ~id =
    raise_lwt (Failure "not implemented")

  let action obj ~id =
    raise_lwt (Failure "not implemented")

  let reset obj ~id =
    raise_lwt (Failure "not implemented")

  let get obj ~id ~address =
    raise_lwt (Failure "not implemented")

  let set obj ~id ~address ~value =
    raise_lwt (Failure "not implemented")

  let reg_set obj ~id ~address ~value =
    raise_lwt (Failure "not implemented")

  let goto obj ~id ~position ~speed ~mode =
    raise_lwt (Failure "not implemented")

  let config obj ~id =
    raise_lwt (Failure "not implemented")

  let get_position obj ~id =
    raise_lwt (Failure "not implemented")

  let get_speed obj ~id =
    raise_lwt (Failure "not implemented")

  let interface =
    make {
      m_ping = (
        fun obj id ->
          let id = Int32.to_int id in
          lwt () = ping (OBus_object.get obj) id in
          return ()
      );
      m_action = (
        fun obj id ->
          let id = Int32.to_int id in
          lwt () = action (OBus_object.get obj) id in
          return ()
      );
      m_reset = (
        fun obj id ->
          let id = Int32.to_int id in
          lwt () = reset (OBus_object.get obj) id in
          return ()
      );
      m_get = (
        fun obj (id, address) ->
          let id = Int32.to_int id in
          let address = make_readable_register address in
          lwt value = get (OBus_object.get obj) id address in
          let value = Int32.of_int value in
          return value
      );
      m_set = (
        fun obj (id, address, value) ->
          let id = Int32.to_int id in
          let address = make_writable_register address in
          let value = Int32.to_int value in
          lwt () = set (OBus_object.get obj) id address value in
          return ()
      );
      m_reg_set = (
        fun obj (id, address, value) ->
          let id = Int32.to_int id in
          let address = make_writable_register address in
          let value = Int32.to_int value in
          lwt () = reg_set (OBus_object.get obj) id address value in
          return ()
      );
      m_goto = (
        fun obj (id, position, speed, mode) ->
          let id = Int32.to_int id in
          let position = Int32.to_int position in
          let speed = Int32.to_int speed in
          let mode = make_exec_mode mode in
          lwt () = goto (OBus_object.get obj) id position speed mode in
          return ()
      );
      m_config = (
        fun obj id ->
          let id = Int32.to_int id in
          lwt () = config (OBus_object.get obj) id in
          return ()
      );
      m_get_position = (
        fun obj id ->
          let id = Int32.to_int id in
          lwt position = get_position (OBus_object.get obj) id in
          let position = Int32.of_int position in
          return position
      );
      m_get_speed = (
        fun obj id ->
          let id = Int32.to_int id in
          lwt speed = get_speed (OBus_object.get obj) id in
          let speed = Int32.of_int speed in
          return speed
      );
    }

  let make () =
    let obus = OBus_object.make ~interfaces:[interface] (device_path "RX64") in
    OBus_object.attach obus ();
    obus
end

module Infrareds =
struct
  open Krobot_dbus_driver.Fr_krobot_LowLevel_Infrareds

  let get env =
    return [0; 0; 0; 0]

  let interface =
    make {
      m_get = (
        fun obj () ->
          lwt states = get (OBus_object.get obj) in
          let states = List.map Int32.of_int states in
          return states
      );
    }

  let make env =
    let obus = OBus_object.make ~interfaces:[interface] (device_path "Infrareds") in
    OBus_object.attach obus env;
    obus
end

module LCD =
struct
  open Krobot_dbus_driver.Fr_krobot_LowLevel_LCD

  type t = {
    ui : Krobot_ui.window;
    chars : char array array;
    mutable line : int;
    mutable column : int;
    mutable cursor : bool;
    mutable backlight : bool;
  }

  let lines = 3
  let columns = 20
  let inter = 4.
  let border = 2.

  type colors = {
    background : float * float * float;
    text_background : float * float * float;
    text_foreground : float * float * float;
  }

  let colors_light = {
    background = (0.4, 0.4, 1.0);
    text_background = (0.0, 0.0, 0.7);
    text_foreground = (1.0, 1.0, 1.0);
  }

  let colors_dark = {
    background = (0.1, 0.1, 0.25);
    text_background = (0.0, 0.0, 0.7 /. 4.);
    text_foreground = (0.25, 0.25, 0.25);
  }

  let set_color ctx (r, g, b) =
    Cairo.set_source_rgb ctx r g b

  let draw lcd =
    let colors = if lcd.backlight then colors_light else colors_dark in
    let { Gtk.width; Gtk.height } = lcd.ui#lcd#misc#allocation in
    let surface = Cairo.image_surface_create Cairo.FORMAT_ARGB32 width height in
    let ctx = Cairo.create surface in
    Cairo.select_font_face ctx "Monospace" Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL;
    Cairo.set_font_size ctx 20.;
    set_color ctx colors.background;
    Cairo.rectangle ctx 0. 0. (float width) (float height);
    Cairo.fill ctx;
    let { Cairo.max_x_advance = fw;
          Cairo.font_height = fh;
          Cairo.descent = descent } = Cairo.font_extents ctx in
    for line = 0 to lines - 1 do
      for column = 0 to columns - 1 do
        let x = inter +. (fw +. inter +. border *. 2.0) *. float column
        and y = inter +. (fh +. inter +. border *. 2.0) *. float line in
        set_color ctx colors.text_background;
        Cairo.rectangle ctx x y (fw +. border *. 2.0) (fh +. border *. 2.0);
        Cairo.fill ctx;
        Cairo.move_to ctx (x +. border) (y +. fh -. descent +. border);
        set_color ctx colors.text_foreground;
        Cairo.show_text ctx (Text.char (Char.code lcd.chars.(line).(column)))
      done
    done;
    if lcd.cursor then begin
      let x = inter +. (fw +. inter +. border *. 2.0) *. float lcd.column
      and y = inter +. (fh +. inter +. border *. 2.0) *. float lcd.line in
      set_color ctx colors.text_foreground;
      Cairo.rectangle ctx x y (fw +. border *. 2.0) (fh +. border *. 2.0);
      Cairo.fill ctx
    end;
    let ctx = Cairo_lablgtk.create lcd.ui#lcd#misc#window in
    Cairo.set_source_surface ctx surface 0. 0.;
    Cairo.rectangle ctx 0. 0. (float width) (float height);
    Cairo.fill ctx;
    Cairo.surface_finish surface

  let add_char lcd ch =
    lcd.chars.(lcd.line).(lcd.column) <- ch;
    if lcd.column + 1 < columns then
      lcd.column <- lcd.column + 1
    else if lcd.line + 1 < lines then begin
      lcd.line <- lcd.line + 1;
      lcd.column <- 0
    end else begin
      lcd.line <- 0;
      lcd.column <- 0
    end

  let clear lcd =
    Array.iter (fun line -> Array.fill line 0 columns ' ') lcd.chars;
    lcd.column <- 0;
    lcd.line <- 0;
    draw lcd

  let set_cursor lcd ~state =
    lcd.cursor <- state;
    draw lcd

  let set_backlight lcd ~state =
    lcd.backlight <- state;
    draw lcd

  let goto lcd ~line ~column =
    lcd.line <- line mod lines;
    lcd.column <- column mod columns;
    draw lcd

  let write lcd ~text =
    String.iter (add_char lcd) text;
    draw lcd

  let write_line lcd ~line ~text =
    lcd.line <- line mod lines;
    lcd.column <- 0;
    write lcd text

  let interface =
    make {
      m_clear = (
        fun obj () ->
          clear (OBus_object.get obj);
          return ()
      );
      m_set_cursor = (
        fun obj state ->
          set_cursor (OBus_object.get obj) state;
          return ()
      );
      m_set_backlight = (
        fun obj state ->
          set_backlight (OBus_object.get obj) state;
          return ()
      );
      m_goto = (
        fun obj (line, column) ->
          let line = Int32.to_int line in
          let column = Int32.to_int column in
          goto (OBus_object.get obj) line column;
          return ()
      );
      m_write = (
        fun obj text ->
          write (OBus_object.get obj) ~text;
          return ()
      );
      m_write_line = (
        fun obj (line, text) ->
          let line = Int32.to_int line in
          write_line (OBus_object.get obj) line text;
          return ()
      );
    }

  let make ui =
    let obus = OBus_object.make ~interfaces:[interface] (device_path "LCD") in
    let lcd = {
      ui = ui;
      chars = Array.make_matrix lines columns ' ';
      line = 0;
      column = 0;
      cursor = true;
      backlight = true;
    } in
    OBus_object.attach obus lcd;
    ignore (ui#lcd#event#connect#expose (fun ev -> draw lcd; true));
    obus
end

module Logic_sensors =
struct
  open Krobot_dbus_driver.Fr_krobot_LowLevel_LogicSensors

  let get_state env =
    return [false; false; false; false;
            false; false; false; false;
            false; false; false; false;
            false; false; false; false]

  let interface =
    make {
      m_get_state = (
        fun obj () ->
          lwt logic_sensors = get_state (OBus_object.get obj) in
          return logic_sensors
      );
    }

  let make env =
    let obus = OBus_object.make ~interfaces:[interface] (device_path "LogicSensors") in
    OBus_object.attach obus env;
    obus
end

module Motors =
struct
  open Krobot_dbus_driver.Fr_krobot_LowLevel_Motors

  let move env ~distance ~velocity ~acceleration =
    let vel, acc = if distance >= 0. then
        (abs_float velocity, abs_float acceleration)
      else
        (-. (abs_float velocity), -. (abs_float acceleration))
    in
    Krobot_env.move env distance vel acc;
    return ()

  let turn env ~angle ~velocity ~acceleration =
    let vel, acc = if angle >= 0. then
        (abs_float velocity, abs_float acceleration)
      else
        (-. (abs_float velocity), -. (abs_float acceleration))
    in
    Krobot_env.turn env angle vel acc;
    return ()

  let stop env ~motor ~mode =
    Krobot_env.set_velocities env 0. 0. 0.;
    return ()

  let set_velocities env ~velocity_r ~velocity_l ~duration =
    Krobot_env.set_velocities env velocity_r velocity_l duration;
    return ()

  let get_current_velocities env =
    let (l_v, r_v) = Krobot_env.get_velocities env in
    return (r_v, l_v)

  let get_current_positions env =
    let (d_l, d_r) = Krobot_env.get_encoders env in
    return (d_l, d_r)

  let get_config obj =
    raise_lwt (Failure "not implemented")

  let set_config obj ~motor ~kp ~ki ~kd ~li =
    raise_lwt (Failure "not implemented")

  let interface =
    make {
      m_move = (
        fun obj (distance, velocity, acceleration) ->
          lwt () = move (OBus_object.get obj) distance velocity acceleration in
          return ()
      );
      m_turn = (
        fun obj (angle, velocity, acceleration) ->
          lwt () = turn (OBus_object.get obj) angle velocity acceleration in
          return ()
      );
      m_stop = (
        fun obj (motor, mode) ->
          let motor = make_motor motor in
          let mode = make_stop_mode mode in
          lwt () = stop (OBus_object.get obj) motor mode in
          return ()
      );
      m_set_velocities = (
        fun obj (velocity_r, velocity_l, duration) ->
          lwt () = set_velocities (OBus_object.get obj) velocity_r velocity_l duration in
          return ()
      );
      m_get_current_velocities = (
        fun obj () ->
          lwt (velocity_r, velocity_l) = get_current_velocities (OBus_object.get obj) in
          return (velocity_r, velocity_l)
      );
      m_get_current_positions = (
        fun obj () ->
          lwt (position_r, position_l) = get_current_positions (OBus_object.get obj) in
          return (position_r, position_l)
      );
      m_get_config = (
        fun obj () ->
          lwt (kp_r, ki_r, kd_r, li_r, kp_l, ki_l, kd_l, li_l) = get_config (OBus_object.get obj) in
          let kp_r = Int32.of_int kp_r in
          let ki_r = Int32.of_int ki_r in
          let kd_r = Int32.of_int kd_r in
          let li_r = Int32.of_int li_r in
          let kp_l = Int32.of_int kp_l in
          let ki_l = Int32.of_int ki_l in
          let kd_l = Int32.of_int kd_l in
          let li_l = Int32.of_int li_l in
          return (kp_r, ki_r, kd_r, li_r, kp_l, ki_l, kd_l, li_l)
      );
      m_set_config = (
        fun obj (motor, kp, ki, kd, li) ->
          let motor = make_motor motor in
          let kp = Int32.to_int kp in
          let ki = Int32.to_int ki in
          let kd = Int32.to_int kd in
          let li = Int32.to_int li in
          lwt () = set_config (OBus_object.get obj) motor kp ki kd li in
          return ()
      );
    }

  let make env =
    let obus = OBus_object.make ~interfaces:[interface] (device_path "Motors") in
    OBus_object.attach obus env;
    obus
end

module Power =
struct
  open Krobot_dbus_driver.Fr_krobot_LowLevel_Power

  let set_buzzer_state obj ~state =
    return ()

  let get_cell_voltage obj =
    return [30; 30; 30; 30; 30; 30; 30; 30]

  let get_current obj =
    return 0

  let get_power_state obj =
    return true

  let get_battery_state obj =
    return `Full

  let interface =
    make {
      m_set_buzzer_state = (
        fun obj state ->
          lwt () = set_buzzer_state (OBus_object.get obj) state in
          return ()
      );
      m_get_cell_voltage = (
        fun obj () ->
          lwt states = get_cell_voltage (OBus_object.get obj) in
          let states = List.map Int32.of_int states in
          return states
      );
      m_get_current = (
        fun obj () ->
          lwt current = get_current (OBus_object.get obj) in
          let current = Int32.of_int current in
          return current
      );
      m_get_power_state = (
        fun obj () ->
          lwt state = get_power_state (OBus_object.get obj) in
          return state
      );
      m_get_battery_state = (
        fun obj () ->
          lwt state = get_battery_state (OBus_object.get obj) in
          let state = cast_battery_state state in
          return state
      );
    }

  let make () =
    let obus = OBus_object.make ~interfaces:[interface] (device_path "Power") in
    OBus_object.attach obus ();
    obus
end

module Range_finders =
struct
  open Krobot_dbus_driver.Fr_krobot_LowLevel_RangeFinders

  let measure env ~id =
    return ()

  let get obj ~id =
    return 0

  let interface =
    make {
      m_measure = (
        fun obj id ->
          let id = Int32.to_int id in
          lwt () = measure (OBus_object.get obj) id in
          return ()
      );
      m_get = (
        fun obj id ->
          let id = Int32.to_int id in
          lwt value = get (OBus_object.get obj) id in
          let value = Int32.of_int value in
          return value
      );
    }

  let make env =
    let obus = OBus_object.make ~interfaces:[interface] (device_path "RangeFinders") in
    OBus_object.attach obus env;
    obus
end

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

let rec draw env window =
  let { Gtk.width; Gtk.height } = window#scene#misc#allocation in
  let surface = Cairo.image_surface_create Cairo.FORMAT_ARGB32 width height in
  let ctx = Cairo.create surface in
  Krobot_env.draw env ctx (float width) (float height);
  let ctx = Cairo_lablgtk.create window#scene#misc#window in
  Cairo.set_source_surface ctx surface 0. 0.;
  Cairo.rectangle ctx 0. 0. (float width) (float height);
  Cairo.fill ctx;
  Cairo.surface_finish surface;
  lwt () = Lwt_unix.sleep 0.04 in
  draw env window

let init bus =
  (* GTK stuff *)

  Lwt_glib.install ();
  ignore (GMain.init ());

  let window = new Krobot_ui.window () in
  ignore (window#window#connect#destroy ~callback:(fun () -> exit 0));
  window#window#show ();
  let env = Krobot_env.create Krobot_env.({x=0.2; y=1.9; theta=(2. *. atan (-1.))}) in
  ignore (draw env window);

  (* OBus stuff *)

  OBus_object.export bus (Analogic_motor.make ());
  OBus_object.export bus (Analogic_servos.make ());
  OBus_object.export bus (Compass.make env);
  OBus_object.export bus (AX12.make ());
  OBus_object.export bus (LCD.make window);
  OBus_object.export bus (Infrareds.make env);
  OBus_object.export bus (Motors.make env);
  OBus_object.export bus (Power.make ());
  OBus_object.export bus (Logic_sensors.make env);
  OBus_object.export bus (Range_finders.make env);
  OBus_object.export bus (RX64.make ());

  let card_interface = Card.make "Interface" in
  let card_sensors = Card.make "Sensors" in
  let card_motors = Card.make "Motors" in
  let card_monitoring = Card.make "Monitoring" in
  let card_rx64 = Card.make "RX64" in

  OBus_object.export bus card_interface;
  OBus_object.export bus card_sensors;
  OBus_object.export bus card_motors;
  OBus_object.export bus card_monitoring;
  OBus_object.export bus card_rx64;

  Lwt_log.notice ~section "ready, waiting for requests"

lwt () = Krobot_daemon.start ~desc:"krobot simulator" ~name:"fr.krobot.Driver" init
