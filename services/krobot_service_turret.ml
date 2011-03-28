(*
 * krobot_service_turret.ml
 * ------------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt
open Krobot_driver
open Krobot_dbus_turret.Fr_krobot_Service_Turret
open Krobot_geometry

type t = {
  obus : t OBus_object.t;
  krobot : Krobot.t;
  data : (float * int) list;
}

let pos_0 = 515.0
let pos_pi_div_2 = 205.0

let position_of_angle angle = truncate (angle /. (pi /. 2.0) *. (pos_0 -. pos_pi_div_2))
let angle_of_position position = float_of_int position /. (pos_0 -. pos_pi_div_2) *. (pi /. 2.0)

let set_ax12 krobot position speed =
  let dev = ax12 krobot in
  lwt () = Dynamixel.goto dev ~id:1 ~position ~speed ~mode:`Now in
  let rec loop () =
    lwt current_position = Dynamixel.get_position dev ~id:1 in
    let delta = abs (current_position - position) in
    lwt () = Lwt_unix.sleep 0.1 in
    if delta < 15 then
      return ()
    else
      loop ()
  in
  loop ()

let goto obj ~angle =
  let angle = principal angle in
  if angle < -.(pi /. 2.0) || angle > pi /. 2.0 then
    raise_lwt (OBus_error.Invalid_args "the angle for the turret must be between -pi/2 and pi/2")
  else
    set_ax12 obj.krobot (position_of_angle angle) 0

let find_distance data measured_value =
  let delta, pos =
    List.fold_left
      (fun (min_delta, pos_at_min) (pos, value) ->
         let delta = abs (measured_value - value) in
         if delta < min_delta then
           (delta, pos)
         else
           (min_delta, pos_at_min))
      (max_int, 0.0)
      data
  in
  if delta > 100 then
    -1.0
  else
    pos

let scan obj =
  let infrareds = Krobot_sensors.infrareds obj.krobot in
  lwt current_position = Dynamixel.get_position (ax12 obj.krobot) ~id:1 in
  let angle = angle_of_position current_position in
  lwt goal =
    if angle > 0.0 then
      lwt () = goto obj (pi /. 2.0) in
      return (-.(pi /. 2.0))
    else
      lwt () = goto obj (-.(pi /. 2.0)) in
      return (pi /. 2.0)
  in
  let thread = set_ax12 obj.krobot (position_of_angle goal) 100 in
  let rec loop acc =
    if state thread <> Sleep then
      return acc
    else begin
      lwt position = Dynamixel.get_position (ax12 obj.krobot) ~id:1 in
      lwt () = Lwt_unix.sleep 0.1 in
      lwt measures = OBus_property.get infrareds in
      loop ((angle_of_position position, find_distance obj.data measures.(0)) :: acc)
    end
  in
  loop []

let find obj =
  raise_lwt (Failure "not implemented")

let interface =
  make {
    m_goto = (
      fun obj angle ->
        goto (OBus_object.get obj) angle
    );
    m_scan = (
      fun obj () ->
        scan (OBus_object.get obj)
    );
    m_find = (
      fun obj () ->
        find (OBus_object.get obj)
    );
  }

let load_data file_name =
  Lwt_stream.to_list
    (Lwt_stream.map
       (fun line -> Scanf.sscanf line "%f %d" (fun position measure -> (position, measure)))
       (Lwt_io.lines_of_file file_name))

let data =
  Krobot_arg.string_d
    ~key:"-data"
    ~doc:"file containing calibration data for the infrared"
    ~default:"/home/krobot/var/infrareds"
    ()

let init bus =
  lwt krobot = Krobot.create () in
  lwt data = load_data (Lazy.force data) in
  let obj = {
    obus = OBus_object.make ~interfaces:[interface] ["fr"; "krobot"; "Services"; "Turret"];
    krobot = krobot;
    data = data;
  } in
  OBus_object.attach obj.obus obj;
  OBus_object.export bus obj.obus;
  return ()

lwt () = Krobot_daemon.start ~desc:"turret control" ~name:"fr.krobot.Service.Turret" init
