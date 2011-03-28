(*
 * krobot_record_infrared.ml
 * -------------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* Record measures of the infrared sensors at each centimerters *)

open Lwt
open Lwt_io

let step = 0.001

let measure_infrared krobot infrareds =
  let rec loop acc = function
    | 0 ->
        return (acc / 20)
    | n ->
        let measure = (React.S.value infrareds).(0) in
        let acc = acc + measure in
        lwt () = Lwt_unix.sleep 0.05 in
        loop acc (n - 1)
  in
  loop 0 20

lwt () =
  Krobot_arg.parse ();

  lwt () = printl "type Ctrl+C to stop" in
  lwt krobot = Krobot.create () in
  lwt infrareds = OBus_property.monitor (Krobot_sensors.infrareds krobot) in
  lwt oc = Lwt_io.open_file ~mode:Lwt_io.output "infrareds-record" in
  let rec loop dist =
    lwt () = Lwt_unix.sleep 1.0 in
    lwt measure = measure_infrared krobot infrareds in
    lwt () = printlf "at distance %fm: %d" dist measure in
    lwt () = fprintlf oc "%f %d" dist measure in
    lwt () = Lwt_io.flush oc in
    lwt _ = Krobot_motors.move krobot ~distance:(-.step) ~velocity:0.2 ~acceleration:0.4 in
    loop (dist +. step)
  in
  loop 0.0
