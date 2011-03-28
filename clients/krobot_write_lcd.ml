(*
 * write_lcd.ml
 * ------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt

lwt () =
  Krobot_arg.parse ();
  try_lwt
    lwt krobot = Krobot.create () in
    lwt _ =
      Lwt_list.fold_left_s
        (fun num line ->
           lwt () =
             Krobot_driver.LCD.write_line
               (Krobot_driver.lcd krobot)
               num
               (String.sub line 0 (min 20 (String.length line)))
           in
           return (num + 1))
        0 (List.tl (Array.to_list Sys.argv))
    in
    return ()
  with exn ->
    lwt () = Lwt_log.error ~exn "failure" in
    exit 0
