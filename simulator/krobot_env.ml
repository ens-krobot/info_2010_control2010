(*
 * krobot_env.ml
 * -------------
 * Copyright : (c) 2010, Xavier Lagorce <Xavier.Lagorce@crans.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open React

type state = {
  x : float;
  y : float;
  theta : float;
}

type internal_state = {
  theta_l : float;
  theta_r : float;
}

type command =
  | Speed of float * float * float * float (* start_time, end_time, left_velocity, right_velocity *)
  | Turn of float * float * float * float (* start_time, t_acc, end_time, velocity *)
  | Move of float * float * float * float (* start_time, t_acc, end_time, velocity *)

type color =
  | Black
  | White
  | Green
  | Red
  | Blue
  | Yellow

type t = {
  mutable state : state;
  mutable internal_state : internal_state;
  mutable command : command;
}

let world_height = 2.1
let world_width = 3.
let robot_size = 0.3
let wheels_diam = 0.098
let wheels_dist = 0.259
let sim_step = 0.01
let time = ref 0.

let velocities_of_command cmd time =
  match cmd with
    | Speed (start, tend, l_v, r_v) ->
      if time < start || time > tend then
        (0., 0.)
      else
        ((l_v +. r_v) *. wheels_diam /. 4., (l_v -. r_v) *. wheels_diam /. wheels_dist)
    | Turn (start, t_acc, tend, vel) ->
      if time < start || time > tend then
        (0., 0.)
      else if time < (start +. t_acc) then
        (0., vel *. (time -. start) /. t_acc)
      else if time < (tend -. t_acc) then
        (0., vel)
      else
        (0., vel *. (tend -. time) /. t_acc)
    | Move (start, t_acc, tend, vel) ->
      if time < start || time > tend then
        (0., 0.)
      else if time < (start +. t_acc) then
        (vel *. (time -. start) /. t_acc, 0.)
      else if time < (tend -. t_acc) then
        (vel, 0.)
      else
        (vel *. (tend -. time) /. t_acc, 0.)

let rec simu_loop env =
  time := !time +. sim_step;
  let (u1, u2) = velocities_of_command env.command !time in
  let dx = u1 *. (cos env.state.theta)
  and dy = u1 *. (sin env.state.theta)
  and dtheta = u2 in
  env.state <- {
    x = env.state.x +. dx *. sim_step;
    y = env.state.y +. dy *. sim_step;
    theta = env.state.theta +. dtheta *. sim_step;
  };
  env.internal_state <- {
    theta_l = env.internal_state.theta_l +. sim_step *. (u1 *. 4. +. u2 *. wheels_dist) /. (2. *. wheels_diam);
    theta_r = env.internal_state.theta_r +. sim_step *. (u1 *. 4. -. u2 *. wheels_dist) /. (2. *. wheels_diam);
  };
  lwt () = Lwt_unix.sleep sim_step in
  simu_loop env

let create state =
  let env = {
    state = state;
    internal_state = {theta_l = 0.; theta_r = 0.};
    command = Speed (0.,0., 0., 0.);
  } in
  ignore (simu_loop env);
  env

let move env distance velocity acceleration =
  let t_acc = velocity /. acceleration in
  let t_end = (velocity *. velocity +. distance *. acceleration) /. (velocity *. acceleration) in
  env.command <-
    if t_end > 2. *. t_acc
    then
      Move (!time, t_acc, !time +. t_end, velocity)
    else begin
      let t_acc = sqrt (distance /. acceleration) in
      let t_end = 2. *. t_acc in
      let velocity = acceleration *. t_acc in
      Move (!time, t_acc, !time +. t_end, velocity)
    end

let turn env angle velocity acceleration =
  let t_acc = velocity /. acceleration in
  let t_end = (velocity *. velocity +. angle *. acceleration) /. (velocity *. acceleration) in
  env.command <-
    if t_end > 2. *. t_acc
    then
      Turn (!time, t_acc, !time +. t_end, velocity)
    else begin
      let t_acc = sqrt (angle /. acceleration) in
      let t_end = 2. *. t_acc in
      let velocity = acceleration *. t_acc in
      Turn (!time, t_acc, !time +. t_end, velocity)
    end

let set_velocities env left_velocity right_velocity duration =
  env.command <- Speed (!time, !time +. duration, left_velocity, right_velocity)

let get_velocities env =
  let (u1, u2) = velocities_of_command env.command !time in
  let l_v = (4. *. u1 +. wheels_dist *. u2) /. (2. *. wheels_diam)
  and r_v = (4. *. u1 -. wheels_dist *. u2) /. (2. *. wheels_diam) in
  (l_v, r_v)

let get_state env =
  env.state

let get_encoders env =
  let (theta_l, theta_r) = (env.internal_state.theta_l, env.internal_state.theta_r) in
  (theta_l *. wheels_diam /. 2., theta_r *. wheels_diam /. 2.)

let set_color ctx color =
  let r, g, b = match color with
    | Black -> (0., 0., 0.)
    | White -> (255., 255., 255.)
    | Green -> (36., 145., 64.)
    | Red -> (199., 23., 18.)
    | Blue -> (0., 59., 128.)
    | Yellow -> (252., 189., 31.)
  in
  Cairo.set_source_rgb ctx (r /. 255.) (g /. 255.) (b /. 255.)

let pi = 4. *. atan 1.

let draw env ctx width height =
  (* Draw the background *)
  Cairo.rectangle ctx 0. 0. width height;
  set_color ctx White;
  Cairo.fill ctx;

  (* Compute the optimal width and height *)
  let dw, dh =
    if width /. height >= (world_width +. 0.204) /. (world_height +. 0.204) then
      ((world_width +. 0.204) /. (world_height +. 0.204) *. height, height)
    else
      (width, width /. (world_width +. 0.204) *. (world_height +. 0.204))
  in

  (* Translation to have the board at the center and scaling to match the window sizes *)
  let x0 = (width -. dw) /. 2. and y0 = (height -. dh) /. 2. in
  let scale = dw /. (world_width +. 0.204) in
  Cairo.translate ctx (x0 +. 0.102 *. scale) (y0 +. dh -. 0.102 *. scale);
  Cairo.scale ctx scale (-.scale);

  Cairo.set_line_width ctx (1. /. scale);

  (* Draw the borders *)
  Cairo.rectangle ctx (-0.022) (-0.022) (world_width +. 0.044) (world_height +. 0.044);
  set_color ctx Black;
  Cairo.fill ctx;

  (* Draw beacon supports *)
  Cairo.rectangle ctx (-0.102) (-0.102) 0.08 0.08;
  Cairo.fill ctx;

  Cairo.rectangle ctx (-0.102) (world_height /. 2. -. 0.04) 0.08 0.08;
  Cairo.fill ctx;

  Cairo.rectangle ctx (-0.102) (world_height +. 0.022) 0.08 0.08;
  Cairo.fill ctx;

  Cairo.rectangle ctx (world_width +. 0.022) (-0.102) 0.08 0.08;
  Cairo.fill ctx;

  Cairo.rectangle ctx (world_width +. 0.022) (world_height /. 2. -. 0.04) 0.08 0.08;
  Cairo.fill ctx;

  Cairo.rectangle ctx (world_width +. 0.022) (world_height +. 0.022) 0.08 0.08;
  Cairo.fill ctx;

  (* Draw the board background *)
  Cairo.rectangle ctx 0. 0. world_width world_height;
  set_color ctx Green;
  Cairo.fill ctx;

  (* Draw the starting areas *)
  Cairo.rectangle ctx 0. (world_height -. 0.4) 0.4 0.4;
  set_color ctx Red;
  Cairo.fill ctx;
  Cairo.rectangle ctx (world_width -. 0.4) (world_height -. 0.4) 0.4 0.4;
  set_color ctx Blue;
  Cairo.fill ctx;

  (* Draw the paving *)
  for i = 0 to 5 do
    for j = 0 to 5 do
      let x = 0.45 +. 0.35 *. float i
      and y = 0.35 *. float j in
      Cairo.rectangle ctx x y 0.35 0.35;
      set_color ctx (if (i + j) mod 2 = 0 then Red else Blue);
      Cairo.fill ctx
    done
  done;

  (* Draw the bands *)
  set_color ctx Black;

  Cairo.rectangle ctx 0.4 0. 0.05 world_height;
  Cairo.fill ctx;

  Cairo.rectangle ctx (world_width -. 0.45) 0. 0.05 world_height;
  Cairo.fill ctx;

  Cairo.rectangle ctx 0.45 0.33 0.7 0.02;
  Cairo.fill ctx;

  Cairo.rectangle ctx (world_width -. 1.15) 0.33 0.7 0.02;
  Cairo.fill ctx;

  Cairo.rectangle ctx 1.13 0. 0.02 0.35;
  Cairo.fill ctx;

  Cairo.rectangle ctx (world_width -. 1.15) 0. 0.02 0.35;
  Cairo.fill ctx;

  Cairo.rectangle ctx 0.45 0. 0.7 0.12;
  Cairo.fill ctx;

  Cairo.rectangle ctx (world_width -. 1.15) 0. 0.7 0.12;
  Cairo.fill ctx;

  Cairo.rectangle ctx 0.45 0. 0.02 0.25;
  Cairo.fill ctx;

  Cairo.rectangle ctx (world_width -. 0.47) 0. 0.02 0.25;
  Cairo.fill ctx;

  Cairo.move_to ctx 0. (world_height -. 0.4);
  Cairo.rel_line_to ctx 0.4 0.;
  Cairo.stroke ctx;

  Cairo.move_to ctx 0. (world_height -. 0.422);
  Cairo.rel_line_to ctx 0.4 0.;
  Cairo.stroke ctx;

  Cairo.move_to ctx (world_width -. 0.4) (world_height -. 0.4);
  Cairo.rel_line_to ctx 0.4 0.;
  Cairo.stroke ctx;

  Cairo.move_to ctx (world_width -. 0.4) (world_height -. 0.422);
  Cairo.rel_line_to ctx 0.4 0.;
  Cairo.stroke ctx;

  (* Draw circles on bonus cases *)
  Cairo.arc ctx 0.975 0.875 0.05 0. (2. *. pi);
  Cairo.fill ctx;

  Cairo.arc ctx 0.975 1.575 0.05 0. (2. *. pi);
  Cairo.fill ctx;

  Cairo.arc ctx 2.025 0.875 0.05 0. (2. *. pi);
  Cairo.fill ctx;

  Cairo.arc ctx 2.025 1.575 0.05 0. (2. *. pi);
  Cairo.fill ctx;

  Cairo.arc ctx 1.325 0.175 0.05 0. (2. *. pi);
  Cairo.fill ctx;

  Cairo.arc ctx 1.675 0.175 0.05 0. (2. *. pi);
  Cairo.fill ctx;

  (* Draw the robot *)
  Cairo.translate ctx env.state.x env.state.y;
  Cairo.rotate ctx env.state.theta;
  Cairo.rectangle ctx (-. robot_size /. 2.) (-. robot_size /. 2.) robot_size robot_size;
  set_color ctx White;
  Cairo.fill ctx;

  (* Draw an arrow on the robot *)
  Cairo.move_to ctx (-. robot_size /. 4.) 0.;
  Cairo.line_to ctx (robot_size /. 4.) 0.;
  Cairo.line_to ctx 0. (-. robot_size /. 4.);
  Cairo.line_to ctx 0. (robot_size /. 4.);
  Cairo.line_to ctx (robot_size /. 4.) 0.;
  set_color ctx Black;
  Cairo.stroke ctx

