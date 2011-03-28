(*
 * krobot_turn.ml
 * --------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt

lwt () =
  let angle =
    Krobot_arg.float_d
      ~key:"-angle"
      ~doc:"angle to turn, in radiants"
      ~default:0.1
      ()
  and velocity =
    Krobot_arg.float_d
      ~key:"-velocity"
      ~doc:"velocity during the move"
      ~default:0.4
      ()
  and acceleration =
    Krobot_arg.float_d
      ~key:"-acceleration"
      ~doc:"acceleration during the move"
      ~default:0.8
      ()
  in
  Krobot_arg.parse ();

  lwt krobot = Krobot.create () in
  lwt result, distance =
    Krobot_motors.turn krobot
      (Lazy.force angle)
      (Lazy.force velocity)
      (Lazy.force acceleration)
  in
  lwt () = Lwt_io.printlf "%f" distance in
  match result with
    | `Success ->
        exit 10
    | `Stopped ->
        lwt () = Lwt_log.warning "movemenet stopped" in
        exit 11
    | `Inhibited ->
        lwt () = Lwt_log.warning "movemenet inhibited" in
        exit 12
    | `Replaced ->
        lwt () = Lwt_log.warning "movemenet replaced" in
        exit 13
