(*
 * range_finders_stop.ml
 * ---------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Stop the robot on collisions *)

open Lwt

let front = 8
let back = 3

(* Duration of an inhibition: *)
let duration = 1.0

let handle_collide krobot range_finders =
  lwt () =
    if range_finders.(front) <> 0 && range_finders.(front) <= 200 then begin
      Krobot_motors.inhibit_forward krobot duration
    end else
      return ()
  and () =
    if range_finders.(back) <> 0 && range_finders.(back) <= 200 then begin
      Krobot_motors.inhibit_backward krobot duration
    end else
      return ()
  in
  return ()

let init bus =
  lwt krobot = Krobot.create () in

  (* React as soon as possible: *)
  lwt range_finders = OBus_property.monitor (Krobot_sensors.range_finders krobot) in
  Lwt_signal.always_notify_p (handle_collide krobot) range_finders;

  (* Continue the inhibition: *)
  let rec loop () =
    lwt () = handle_collide krobot (React.S.value range_finders) in
    lwt () = Lwt_unix.sleep (duration /. 2.) in
    loop ()
  in
  loop ()

lwt () = Krobot_daemon.start ~desc:"range-finders security" ~name:"fr.krobot.Service.Stopper.RangeFinders" init
