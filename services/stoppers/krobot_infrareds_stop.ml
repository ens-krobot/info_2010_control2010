(*
 * krobot_infrareds_stop.ml
 * ------------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Stop the robot on collisions *)

open Lwt

(* Duration of an inhibition: *)
let duration = 1.0

let front = 1
let back = 2

let front_limit = 1700
let back_limit = 2500

let handle_collide krobot infrareds =
  lwt () =
    if infrareds.(front) > front_limit then begin
      Krobot_motors.inhibit_forward krobot ~duration
    end else
      return ()
  and () =
    if infrareds.(back) > back_limit then begin
      Krobot_motors.inhibit_backward krobot ~duration
    end else
      return ()
  in
  return ()

let init bus =
  lwt krobot = Krobot.create () in

  (* React as soon as possible: *)
  lwt infrareds = OBus_property.monitor (Krobot_sensors.infrareds krobot) in
  Lwt_signal.always_notify_p (handle_collide krobot) infrareds;

  (* Continue the inhibition: *)
  let rec loop () =
    lwt () = handle_collide krobot (React.S.value infrareds) in
    lwt () = Lwt_unix.sleep (duration /. 2.) in
    loop ()
  in
  loop ()

lwt () = Krobot_daemon.start ~desc:"infrared security" ~name:"fr.krobot.Service.Stopper.Infrareds" init
