(*
 * joy_control.ml
 * --------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Control the robot with a joystick *)

open Lwt
open Sdljoystick
open Sdlevent
open Sdlkey

(* +-----------------------------------------------------------------+
   | Joystick events                                                 |
   +-----------------------------------------------------------------+ *)

type button =
  | ButtonCross
  | ButtonSquare
  | ButtonTriangle
  | ButtonCircle
  | ButtonDown
  | ButtonLeft
  | ButtonUp
  | ButtonRight
  | ButtonSelect
  | ButtonStart
  | ButtonR1
  | ButtonR2
  | ButtonL1
  | ButtonL2
  | ButtonPS3
  | ButtonLAxis
  | ButtonRAxis

type event =
  | JoyRAxisV of float
  | JoyLAxisV of float
  | JoyRAxisH of float
  | JoyLAxisH of float
  | JoyButtonPressed of button
  | JoyButtonReleased of button
  | KeyPressed of Sdlkey.t
  | KeyReleased of Sdlkey.t

(* +-----------------------------------------------------------------+
   | int --> button                                                  |
   +-----------------------------------------------------------------+ *)

let raxis_v = 3
let raxis_h = 2
let laxis_v = 1
let laxis_h = 0

let axis_min = -32768.0
let axis_max = 32767.0

let button_of_num = function
  | 14 -> Some ButtonCross
  | 15 -> Some ButtonSquare
  | 12 -> Some ButtonTriangle
  | 13 -> Some ButtonCircle
  | 6 -> Some ButtonDown
  | 7 -> Some ButtonLeft
  | 4 -> Some ButtonUp
  | 5 -> Some ButtonRight
  | 0 -> Some ButtonSelect
  | 3 -> Some ButtonStart
  | 11 -> Some ButtonR1
  | 9 -> Some ButtonR2
  | 10 -> Some ButtonL1
  | 8 -> Some ButtonL2
  | 16 -> Some ButtonPS3
  | 1 -> Some ButtonLAxis
  | 2 -> Some ButtonRAxis
  | n -> None

(* +-----------------------------------------------------------------+
   | SDL events (executed in a child process)                        |
   +-----------------------------------------------------------------+ *)

let child_loop pipe joy =
  let axis_state = Array.make (num_axes joy) 0.0 in
  let send ev =
    Pervasives.output_value pipe ev;
    Pervasives.flush pipe
  in
  while true do
    match wait_event () with
      | KEYDOWN { keysym = key } ->
          send (KeyPressed key);
          if key = KEY_ESCAPE then begin
            Sdl.quit ();
            exit 0
          end
      | JOYAXISMOTION { jae_axis = axis; jae_value = value } ->
          let value = 0.1 -. ((float_of_int value -. axis_min) *. 0.2 /. (axis_max -. axis_min)) in
          if value <> axis_state.(axis) then begin
            axis_state.(axis) <- value;
            if axis = laxis_h then
              send (JoyLAxisH value)
            else if axis = laxis_v then
              send (JoyLAxisV value)
            else if axis = raxis_h then
              send (JoyRAxisH value)
            else if axis = raxis_v then
              send (JoyRAxisV value)
            else
              ()
          end
      | JOYBUTTONUP { jbe_button = button } -> begin
          match button_of_num button with
            | Some button ->
                send (JoyButtonReleased button)
            | None ->
                ()
        end
      | JOYBUTTONDOWN { jbe_button = button } -> begin
          match button_of_num button with
            | Some button ->
                send (JoyButtonPressed button)
            | None ->
                ()
        end
      | _ ->
          ()
  done

(* +-----------------------------------------------------------------+
   | Handling events (in the parent process)                         |
   +-----------------------------------------------------------------+ *)

let axis_coef = 6.0
let axis_coef_turn = 4.0
let duration = 0.2

lwt record_oc =
  if Array.length Sys.argv >= 2 && Sys.argv.(1) = "record" then
    Lwt_io.open_file ~mode:Lwt_io.output "krobot.record"
  else
    return Lwt_io.null

let date_last_command = ref (Unix.gettimeofday ())

let robot_call record f =
  let date = Unix.gettimeofday () in
  let delta = date -. !date_last_command in
  date_last_command := date;
  lwt () = Lwt_io.fprintf record_oc "%Ld\n%s\n" (Int64.bits_of_float delta) record in
  try_lwt
    f ()
  with Failure msg ->
    lwt () = Lwt_log.error_f "action << %s >> failed with: %s" record msg in
    return ()

let rec set_velocities krobot velocity_l velocity_r =
  lwt () = Lwt_log.info_f "set-velocities: left=%f right=%f" velocity_l velocity_r in
  lwt () =
    robot_call
      (Printf.sprintf
         "set-velocities velocity-left=%f velocity-right=%f duration=%f"
         velocity_l velocity_r duration)
      (fun () ->
         Krobot_motors.set_velocities krobot ~velocity_l ~velocity_r ~duration)
  in
  if velocity_l = 0.0 && velocity_r = 0.0 then
    return ()
  else begin
    lwt () = Lwt_unix.sleep (duration /. 2.) in
    set_velocities krobot velocity_l velocity_r
  end

let parent_loop krobot pipe =
  let stop = ref false in
  let thread = ref (return ()) in
  let raxis_h = ref 0.0
  and raxis_v = ref 0.0
  and laxis_h = ref 0.0
  and laxis_v = ref 0.0 in
  let set_velocities () =
    cancel !thread;
    if not !stop then
      thread :=
        set_velocities krobot
          (!laxis_v *. axis_coef -. !raxis_h *. axis_coef_turn)
          (!laxis_v *. axis_coef +. !raxis_h *. axis_coef_turn)
  in
  let enable_claws = lazy(robot_call "claws.enable" (fun () -> Krobot_claws.enable krobot)) in
  let rec loop () =
    Lwt_io.read_value pipe >>= function
      | KeyPressed KEY_ESCAPE ->
          return ()
      | JoyLAxisV n ->
          laxis_v := n;
          set_velocities ();
          loop ()
      | JoyLAxisH n ->
          laxis_h := n;
          set_velocities ();
          loop ()
      | JoyRAxisV n ->
          raxis_v := n;
          set_velocities ();
          loop ()
      | JoyRAxisH n ->
          raxis_h := n;
          set_velocities ();
          loop ()
      | JoyButtonPressed ButtonSquare ->
          stop := true;
          cancel !thread;
          lwt () = robot_call "stop-motors mode=abrupt" (fun () -> Krobot_motors.stop krobot ~mode:`Abrupt) in
          loop ()
      | JoyButtonReleased ButtonSquare ->
          stop := false;
          loop ()
      | JoyButtonPressed ButtonCross ->
          lwt () = Lazy.force enable_claws in
          lwt () = robot_call "claws.take" (fun () -> Krobot_claws.take krobot) in
          loop ()
      | JoyButtonPressed ButtonCircle ->
          lwt () = Lazy.force enable_claws in
          lwt () = robot_call "claws.open" (fun () -> Krobot_claws.open_ krobot) in
          loop ()
      | JoyButtonPressed ButtonTriangle ->
          lwt () = Lazy.force enable_claws in
          lwt () = robot_call "claws.close" (fun () -> Krobot_claws.close krobot) in
          loop ()
      | JoyButtonPressed ButtonR1 ->
          lwt () = robot_call "grip.up" (fun () -> Krobot_grip.up krobot >> return ()) in
          loop ()
      | JoyButtonPressed ButtonR2 ->
          lwt () = robot_call "grip.down" (fun () -> Krobot_grip.down krobot) in
          loop ()
      | JoyButtonPressed ButtonL1 ->
          lwt () = robot_call "grip.close" (fun () -> Krobot_grip.close krobot) in
          loop ()
      | JoyButtonPressed ButtonL2 ->
          lwt () = robot_call "grip.release" (fun () -> Krobot_grip.release krobot) in
          loop ()
      | _ ->
          loop ()
  in
  loop ()

(* +-----------------------------------------------------------------+
   | Entry-point                                                     |
   +-----------------------------------------------------------------+ *)

let () =
  Krobot_arg.parse ();
  let fd_r, fd_w = Unix.pipe () in
  match Unix.fork () with
    | 0 ->
        Unix.close fd_r;
        Sdl.init [`JOYSTICK;`VIDEO];
        Sdljoystick.set_event_state true;
        let joy =
          try
            open_joystick 0
          with exn ->
            Printf.eprintf "cannot open joystick: %s\n%!" (Printexc.to_string exn);
            raise exn
        in
        child_loop (Unix.out_channel_of_descr fd_w) joy
    | pid ->
        Unix.close fd_w;
        Lwt_main.run begin
          lwt krobot = Krobot.create () in
          lwt () = Lwt_log.notice "ready to process event" in
          parent_loop krobot (Lwt_io.of_unix_fd ~mode:Lwt_io.input fd_r)
        end
