(*
 * krobot_service_motors.ml
 * ------------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt
open Krobot_driver
open Krobot_dbus_motors.Fr_krobot_Service_Motors

type t = {
  obus : t OBus_object.t;
  krobot : Krobot.t;

  mutable abort : type_move_result Lwt.u option;

  mutable state : [ `Forward | `Backward | `Other | `Static ];

  inhibited_forward : bool React.signal;
  set_inhibited_forward : bool -> unit;

  inhibited_backward : bool React.signal;
  set_inhibited_backward : bool -> unit;

  mutable inhibit_forward : unit Lwt.t;
  mutable inhibit_backward : unit Lwt.t;
}

let can_move motors direction =
  match direction with
    | `Static ->
        true
    | `Forward ->
        not (React.S.value motors.inhibited_forward)
    | `Backward ->
        not (React.S.value motors.inhibited_backward)
    | `Other ->
        not (React.S.value motors.inhibited_forward || React.S.value motors.inhibited_backward)

let abort motors why =
  match motors.abort with
    | Some wakener ->
        motors.abort <- None;
        wakeup wakener why
    | None ->
        ()

let replace motors =
  abort motors `Replaced;
  let waiter, wakener = task () in
  motors.abort <- Some wakener;
  waiter

let turn motors ~angle ~velocity ~acceleration =
  motors.state <- `Other;
  pick [replace motors;
        (lwt () = Motors.turn (Krobot_driver.motors motors.krobot) ~angle ~velocity ~acceleration in
         motors.state <- `Static;
         return `Success)]

let move motors ~distance ~velocity ~acceleration =
  let direction = if distance > 0.0 then `Forward else `Backward in
  if can_move motors direction then begin
    motors.state <- direction;
    pick [replace motors;
          (lwt () = Motors.move (Krobot_driver.motors motors.krobot) ~distance ~velocity ~acceleration in
           motors.state <- `Static;
           return `Success)]
  end else
    return `Inhibited

let stop motors ~mode =
  abort motors `Stopped;
  motors.state <- `Static;
  Motors.stop (Krobot_driver.motors motors.krobot) ~motor:`Both ~mode

let set_velocities motors ~velocity_r ~velocity_l ~duration =
  let direction =
    match velocity_r > 0.0, velocity_l > 0.0 with
      | true, true -> `Forward
      | false, false -> `Backward
      | _ -> `Other
  in
  if can_move motors direction then begin
    motors.state <- direction;
    pick [(lwt _ = replace motors in
           return ());
          (lwt () = Motors.set_velocities (Krobot_driver.motors motors.krobot) ~velocity_r ~velocity_l ~duration in
           lwt () = Lwt_unix.sleep duration in
           motors.state <- `Static;
           return ())]
  end else
    return ()

let inhibit_forward motors ~duration =
  motors.set_inhibited_forward true;
  cancel motors.inhibit_forward;
  motors.inhibit_forward <- (lwt () = Lwt_unix.sleep duration in
                             motors.set_inhibited_forward false;
                             return ());
  match motors.state with
    | `Forward | `Other ->
        stop motors `Smooth
    | _ ->
        return ()

let inhibit_backward motors ~duration =
  motors.set_inhibited_backward true;
  cancel motors.inhibit_backward;
  motors.inhibit_backward <- (lwt () = Lwt_unix.sleep duration in
                              motors.set_inhibited_backward false;
                              return ());
  match motors.state with
    | `Backward | `Other ->
        stop motors `Smooth
    | _ ->
        return ()

let interface =
  make {
    m_turn = (
      fun obj (angle, velocity, acceleration) ->
        lwt result = turn (OBus_object.get obj) angle velocity acceleration in
        return (cast_move_result result, 0.0)
    );
    m_move = (
      fun obj (distance, velocity, acceleration) ->
        lwt result = move (OBus_object.get obj) distance velocity acceleration in
        return (cast_move_result result, 0.0)
    );
    m_stop = (
      fun obj mode ->
        let mode = make_stop_mode mode in
        stop (OBus_object.get obj) mode
    );
    m_set_velocities = (
      fun obj (velocity_r, velocity_l, duration) ->
        set_velocities (OBus_object.get obj) velocity_r velocity_l duration
    );
    m_inhibit_forward = (
      fun obj duration ->
        inhibit_forward (OBus_object.get obj) duration
    );
    m_inhibit_backward = (
      fun obj duration ->
        inhibit_backward (OBus_object.get obj) duration
    );
    p_inhibited_forward = (fun obj -> (OBus_object.get obj).inhibited_forward);
    p_inhibited_backward = (fun obj -> (OBus_object.get obj).inhibited_backward);
  }

let init bus =
  lwt krobot = Krobot.create () in
  let inhibited_forward, set_inhibited_forward = React.S.create false in
  let inhibited_backward, set_inhibited_backward = React.S.create false in
  let obj = {
    obus = OBus_object.make ~interfaces:[interface] ["fr"; "krobot"; "Services"; "Motors"];
    krobot = krobot;
    abort = None;
    state = `Static;
    inhibited_forward = inhibited_forward;
    set_inhibited_forward = set_inhibited_forward;
    inhibited_backward = inhibited_backward;
    set_inhibited_backward = set_inhibited_backward;
    inhibit_forward = return ();
    inhibit_backward = return ();
  } in
  OBus_object.attach obj.obus obj;
  OBus_object.export bus obj.obus;
  return ()

lwt () =
  Krobot_daemon.start ~desc:"motors control" ~name:"fr.krobot.Service.Motors" init
