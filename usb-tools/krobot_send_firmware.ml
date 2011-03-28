(*
 * krobot_send_firmware.ml
 * -----------------------
 * Copyright : (c) 2009, Stéphane Glondu <steph@glondu.net>
 * Licence   : BSD3
 *
 * This file is a part of Krobot.
 *)

open Lwt
open Lwt_io

let do_flash force filename =
  lwt hex = Hexfile.parse_file filename in
  let memory =
    let buffer = String.make 0x8000 '\255' in
    Hexfile.validate_and_copy hex 0x0 buffer 0 0x8000;
    buffer
  in
  let firmware_name = Boardname.get_board_name memory in
  lwt () = match firmware_name with
    | Some s -> printf "Detected firmware: %S\n" s
    | None -> printf "Unable to identify firmware!\n"
  in
  let address = 0x800 and length = 0x8000-0x800 in
  lwt k = Bootloader.open_card () in
  lwt () = printf "Card opened\n" in
  lwt data = Bootloader.get_flash k ~address:0x0 ~length:0x8000 in
  let board_name = Boardname.get_board_name data in
  lwt () = match board_name with
    | Some s -> printf "Detected card: %S\n" s
    | None -> printf "Unable to identify card!\n"
  in
  lwt () =
      if not force && (board_name = None || firmware_name = None || board_name <> firmware_name) then begin
        lwt () = eprintf "board name and firmware name do not match, use --force\n" in
        exit 1
      end else return ()
  in
  lwt () = Bootloader.erase_flash k ~address ~length in
  lwt () = printf "Flash erased\n" in
  lwt () = Bootloader.write_flash k ~address memory address length in
  lwt () = printf "Flashing completed\n" in
  lwt () = Bootloader.reset_board k in
  return ()

lwt () =
  let force = ref false in
  let filename = ref None in
  let speclist = [
    "--force", Arg.Set force, "Force flashing even if board id and firmware id do not match";
  ] in
  Arg.parse speclist
    (fun s ->
       match !filename with
         | None -> filename := Some s
         | Some _ -> raise (Arg.Bad s))
    "Send a firmware to a board in Bootloader mode";
  let filename = match !filename with
    | None -> Printf.eprintf "You must specify a .hex file!\n"; exit 1
    | Some s -> s
  in
  try_lwt
    do_flash !force filename
  with
    | Bootloader.Error e ->
        eprintl (Bootloader.string_of_error e)
    | e ->
        eprintl (Printexc.to_string e)
