(*
 * krobot_dump_memory.ml
 * ---------------------
 * Copyright : (c) 2009, Stéphane Glondu <steph@glondu.net>
 * Licence   : BSD3
 *
 * This file is a part of Krobot.
 *)

open Printf
open Lwt
open Lwt_io

lwt () =
  lwt k = Bootloader.open_card () in
  try_lwt
    lwt data = Bootloader.get_flash k ~address:0x0 ~length:0x8000 in
    let msg = match Boardname.get_board_name data with
      | Some s -> sprintf "Board: %S" s
      | None -> "Unable to identify board!"
    in
    lwt () = eprintlf "%s" msg in
    (if Unix.isatty Unix.stdout then hexdump else write) stdout data >> flush stdout
  with
    | Bootloader.Error e ->
        eprintlf "%s" (Bootloader.string_of_error e)
    | e ->
        eprintlf "%s" (Printexc.to_string e)
