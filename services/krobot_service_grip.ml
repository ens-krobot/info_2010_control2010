(*
 * krobot_service_grip.ml
 * ----------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt
open Krobot_driver
open Krobot_dbus_grip.Fr_krobot_Service_Grip

type t = {
  obus : t OBus_object.t;
  krobot : Krobot.t;
}

let set_ax12 krobot actions =
  let dev = ax12 krobot in
  lwt () =
    Lwt_list.iter_p
      (fun (id, position, speed) ->
         Dynamixel.goto dev ~id ~position ~speed ~mode:`Action)
      actions
  in
  lwt () = Dynamixel.action dev ~id:0xfe in
  let rec loop () =
    lwt delta =
      Lwt_list.fold_left_s
        (fun delta (id, position, speed) ->
           lwt current_position = Dynamixel.get_position dev id in
           return (max delta (abs (current_position - position))))
        0 actions
    in
    lwt () = Lwt_unix.sleep 0.1 in
    if delta < 15 then
      return ()
    else
      loop ()
  in
  loop ()

let set_rx64 krobot position velocity =
  let dev = rx64 krobot in
  lwt () = Dynamixel.set dev ~id:1 ~address:`Moving_speed ~value:velocity in
  lwt () = Dynamixel.set dev ~id:1 ~address:`Goal_position ~value:position in
  let rec loop () =
    lwt current_position = Dynamixel.get_position dev ~id:1 in
    lwt () = Lwt_unix.sleep 0.1 in
    if abs (current_position - position) < 15 then
      return ()
    else
      loop ()
  in
  loop ()

let open_ obj =
  set_ax12 obj.krobot [(2, 650, 100);
                        (3, 0, 0)]

let up obj =
  let thread = set_rx64 obj.krobot 222 0 <&> set_ax12 obj.krobot [(2, 180, 200)] in
  lwt () = Lwt_unix.sleep 0.5 in
  lwt position = Dynamixel.get_position (rx64 obj.krobot) ~id:1 in
  (* Test whether the grip succeed is raised up the ear *)
  if position > 520 then begin
    (* On failure, disable the torque an open the grip *)
    cancel thread;
    lwt () = Dynamixel.set (rx64 obj.krobot) ~id:1 ~address:`Torque_enable ~value:0 in
    lwt () = open_ obj in
    return false
  end else
    lwt () = thread in
    return true

let down obj =
  lwt () = set_rx64 obj.krobot 530 200
  and () = set_ax12 obj.krobot [(2, 510, 200);
                                (3, 390, 200)]
  in
  return ()

let close obj =
  set_ax12 obj.krobot [(2, 572, 200);
                        (3, 371, 200)]

let release obj =
  set_ax12 obj.krobot [(2, 200, 0)]

let interface =
  make {
    m_up = (
      fun obj () ->
        up (OBus_object.get obj)
    );
    m_down = (
      fun obj () ->
        down (OBus_object.get obj)
    );
    m_open = (
      fun obj () ->
        open_ (OBus_object.get obj)
    );
    m_close = (
      fun obj () ->
        close (OBus_object.get obj)
    );
    m_release = (
      fun obj () ->
        release (OBus_object.get obj)
    );
  }

let init bus =
  lwt krobot = Krobot.create () in
  let obj = {
    obus = OBus_object.make ~interfaces:[interface] ["fr"; "krobot"; "Services"; "Grip"];
    krobot = krobot;
  } in
  OBus_object.attach obj.obus obj;
  OBus_object.export bus obj.obus;
  return ()

lwt () = Krobot_daemon.start ~desc:"grip control" ~name:"fr.krobot.Service.Grip" init
