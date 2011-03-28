(*
 * krobot_service_sensors.ml
 * -------------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt
open Krobot_driver
open Krobot_dbus_sensors.Fr_krobot_Service_Sensors

type t = {
  obus : t OBus_object.t;
  krobot : Krobot.t;

  color : [ `Yellow | `Blue ] React.signal;
  infrareds : int list React.signal;
  logic_sensors : bool list React.signal;
  range_finders : int list React.signal;
}

let poll ?(delay=0.05) f initial =
  React.S.hold initial
    (Lwt_event.from
       (fun () ->
          let rec loop () =
            lwt () = Lwt_unix.sleep delay in
            try_lwt
              f ()
            with exn ->
              loop ()
          in
          loop ()))

let interface =
  make {
    p_color = (fun obj -> React.S.map cast_color (OBus_object.get obj).color);
    p_infrareds = (fun obj -> React.S.map (List.map Int32.of_int) (OBus_object.get obj).infrareds);
    p_logic_sensors = (fun obj -> (OBus_object.get obj).logic_sensors);
    p_range_finders = (fun obj -> React.S.map (List.map Int32.of_int) (OBus_object.get obj).range_finders);
  }

let init bus =
  lwt krobot = Krobot.create () in
  let obj = {
    obus = OBus_object.make ~interfaces:[interface] ["fr"; "krobot"; "Services"; "Sensors"];
    krobot = krobot;
    color =
      poll ~delay:1.0
        (fun () ->
           lwt ports = Krobot_driver.USB_card.get_ports_state (Krobot_driver.card_interface krobot) in
           if List.nth ports 3 land 64 = 0 then
             return `Yellow
           else
             return `Blue)
        `Blue;
    infrareds =
      poll
        (fun () -> Infrareds.get (infrareds krobot))
        [0; 0; 0; 0];
    logic_sensors =
      poll
        (fun () -> Logic_sensors.get_state (logic_sensors krobot))
        [false; false; false; false;
         false; false; false; false];
    range_finders =
      poll
        (fun () ->
           let dev = range_finders krobot in
           let a = Array.create 8 0 in
           lwt () =
             for_lwt i = 0 to 7 do
               lwt () = Range_finders.measure dev i in
               lwt v = Range_finders.get dev i in
               a.(i) <- v;
               return ()
             done
           in
           return (Array.to_list a))
        [0; 0; 0; 0; 0; 0; 0; 0];
  } in
  OBus_object.attach obj.obus obj;
  OBus_object.export bus obj.obus;
  return ()

lwt bus = Krobot_daemon.start ~desc:"sensors measurement" ~name:"fr.krobot.Service.Sensors" init
