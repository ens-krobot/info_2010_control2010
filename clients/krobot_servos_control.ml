(*
 * krobot_servos_control.ml
 * ------------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(* AX12/RX64 interactive controller *)

open Lwt
open Lwt_io
open Lwt_term

let render dynamixels positions selected =
  for_lwt i = 0 to 3 do
    let name, dev, id = dynamixels.(i) in
    let line = Printf.sprintf "%s.(%d) : %4d" name id positions.(i) in
    if i = selected then
      printlc [inverse; text line]
    else
      printl line
  done

lwt () =
  Krobot_arg.parse ();

  lwt krobot = Krobot.create () in

  let ax12 = Krobot_driver.ax12 krobot and rx64 = Krobot_driver.rx64 krobot in
  let dynamixels = [|("ax12", ax12, 1); ("ax12", ax12, 2); ("ax12", ax12, 3); ("rx64", rx64, 1)|] in

  lwt () = Lwt_log.notice "reading current servos positions" in
  let positions = [|0; 0; 0 ;0|] in
  lwt () =
    for_lwt i = 0 to 3 do
      let name, dev, id = dynamixels.(i) in
      lwt position = Krobot_driver.Dynamixel.get_position dev id in
      positions.(i) <- position;
      return ()
    done
  in

  lwt () = printl "use arrow keys to control the servos" in

  let rec loop selected =
    lwt () = render dynamixels positions selected in
    Lwt_term.read_key () >>= function
      | Key_up ->
          if selected > 0 then
            loop2 (selected - 1)
          else
            loop2 selected
      | Key_down ->
          if selected < Array.length positions - 1 then
            loop2 (selected + 1)
          else
            loop2 selected
      | Key_left ->
          positions.(selected) <- max 0 (positions.(selected) - 10);
          let name, dev, id = dynamixels.(selected) in
          lwt () = Krobot_driver.Dynamixel.goto dev ~id ~position:positions.(selected) ~speed:50 ~mode:`Now in
          loop2 selected
      | Key_right ->
          positions.(selected) <- min 1023 (positions.(selected) + 10);
          let name, dev, id = dynamixels.(selected) in
          lwt () = Krobot_driver.Dynamixel.goto dev ~id ~position:positions.(selected) ~speed:50 ~mode:`Now in
          loop2 selected
      | Key ("\027[d" | "\027[1;2D") ->
          positions.(selected) <- max 0 (positions.(selected) - 1);
          let name, dev, id = dynamixels.(selected) in
          lwt () = Krobot_driver.Dynamixel.goto dev ~id ~position:positions.(selected) ~speed:50 ~mode:`Now in
          loop2 selected
      | Key ("\027[c" | "\027[1;2C") ->
          positions.(selected) <- min 1023 (positions.(selected) + 1);
          let name, dev, id = dynamixels.(selected) in
          lwt () = Krobot_driver.Dynamixel.goto dev ~id ~position:positions.(selected) ~speed:50 ~mode:`Now in
          loop selected
      | Key_control '[' ->
          return ()
      | _ ->
          loop2 selected
  and loop2 selected =
    lwt () = goto_beginning_of_line 4 in
    loop selected
  in

  lwt () = Lwt_term.enter_drawing_mode () in
  lwt () = Lwt_term.hide_cursor () in

  try_lwt
    loop 0
  finally
    Lwt_term.leave_drawing_mode ()
