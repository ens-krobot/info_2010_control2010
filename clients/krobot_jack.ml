(*
 * krobot_jack.ml
 * --------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Wait for the jack to be removed *)

open Lwt

lwt () =
  Krobot_arg.parse ();
  lwt krobot = Krobot.create () in
  let waiter, wakener = wait () in
  lwt logic_sensors = OBus_property.monitor (Krobot_sensors.logic_sensors krobot) in
  Lwt_signal.always_notify (fun ls -> if not ls.(1) then wakeup wakener ()) logic_sensors;
  waiter
