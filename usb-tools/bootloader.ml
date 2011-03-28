(*
 * krobot_Bootloader.ml
 * --------------------
 * Copyright : (c) 2009, Stéphane Glondu <steph@glondu.net>
 * Licence   : BSD3
 *
 * This file is a part of Krobot.
 *)

open Lwt
open Lwt_io

(* Code très inspiré de kard.ml, mais mis à part car le mode de
   communication avec la carte est différent (bulk au lieu
   d'interrupt). *)

type t = {
  mutable is_open : bool;
  (* La carte est-elle ouverte ? *)

  handle : USB.handle;
  (* Handle pour le périphérique usb *)

  kernel_active : bool;
  (* Est-ce qu'un driver noyau était attaché à la carte avant qu'on
     l'utilise ? *)
}

type error =
  | IncompleteWrite of int * int
  | IncompleteRead of int * int
  | UnexpectedReply of string * string
  | WriteError of string * string

let string_of_error = function
  | IncompleteWrite (a, b) -> Printf.sprintf "%d byte(s) written instead of %d" a b
  | IncompleteRead (a, b) -> Printf.sprintf "%d byte(s) read instead of %d" a b
  | UnexpectedReply (a, b) -> Printf.sprintf "received unexpected reply %S instead of %S" a b
  | WriteError (a, b) -> Printf.sprintf "written: %S, read back: %S" a b

exception Error of error

let failwith e = raise_lwt (Error e)

let close k =
  if k.is_open then begin
    lwt _ = USB.release_interface k.handle 0 in
    lwt _ = USB.reset_device k.handle in
    (*if k.kernel_active then USB.attach_kernel_driver k.handle 0;*)
    (*USB.close k.handle;*)
    k.is_open <- false;
    return ()
 end else return ()

let open_card () =
  let handle = USB.open_device_with
    ~vendor_id:PcInterface.usb_vid
    ~product_id:PcInterface.usb_pid_bootloader
  in
  let kernel_active = USB.kernel_driver_active handle 0 in
  if kernel_active then USB.detach_kernel_driver handle 0;
  lwt _ = USB.set_configuration handle 1 in
  lwt _ = USB.claim_interface handle 0 in
  let k = { is_open = true;
            handle = handle;
            kernel_active = kernel_active } in
  let _ = Lwt_sequence.add_l (fun _ -> close k) Lwt_main.exit_hooks in
  return k

let header_length = 5

let put_message buffer cmd length address data =
  let body_length = String.length data in
  assert (String.length buffer >= header_length+body_length);
  let set i n = buffer.[i] <- char_of_int n in
  set 0 cmd;
  assert (length < 0x100);
  set 1 length;
  assert (address <= 0x1000000);
  set 2 (address land 0xff);
  set 3 ((address lsr 8) land 0xff);
  set 4 ((address lsr 16) land 0xff);
  String.blit data 0 buffer 5 body_length

let send_receive_packet k send_buffer send_length receive_buffer receive_length send_delay receive_delay =
  let handle = k.handle and endpoint = 1 in
  lwt sent = USB.bulk_send ~handle ~endpoint ~timeout:1. send_buffer 0 send_length in
  if sent <> send_length then
    failwith (IncompleteWrite (sent, send_length))
  else begin
    lwt received = USB.bulk_recv ~handle ~endpoint ~timeout:3. receive_buffer 0 receive_length in
    if received <> receive_length then
      failwith (IncompleteRead (received, receive_length))
    else
      return ()
  end

let get_flash k ~address ~length =
  let response_length = 64 in
  let increment = response_length-header_length in
  assert (increment < 256);
  let send_buffer = String.create header_length in
  let receive_buffer = String.create response_length in
  let result_buffer = String.create length in
  let rec loop offset total_length =
    if total_length <= 0 then
      return result_buffer
    else begin
      let length = min increment total_length in
      let response_length = length+header_length in
      let address = address+offset in
      put_message send_buffer PcInterface.read_flash length address "";
      lwt () = send_receive_packet k send_buffer header_length receive_buffer response_length 1. 3. in
      let receive_header = String.sub receive_buffer 0 header_length in
      if receive_header <> send_buffer then
        failwith (UnexpectedReply (receive_header, send_buffer))
      else begin
        String.blit receive_buffer header_length result_buffer offset length;
        loop (offset+length) (total_length-length);
      end
    end
  in loop 0 length

let erase_flash k ~address ~length =
  let response_length = 1 in
  (* les effacements se font par blocs de 64 octets *)
  let increment = 64 in
  let send_buffer = String.create header_length in
  let receive_buffer = String.create response_length in
  let rec loop offset total_length =
    if total_length <= 0 then
      return ()
    else begin
      let address = address+offset in
      put_message send_buffer PcInterface.erase_flash 1 address "";
      lwt () = send_receive_packet k send_buffer header_length receive_buffer response_length 1. 5. in
      if int_of_char receive_buffer.[0] <> PcInterface.erase_flash then
        failwith (UnexpectedReply (receive_buffer, String.make 1 (char_of_int PcInterface.erase_flash)))
      else
        loop (offset+increment) (total_length-increment);
    end
  in loop 0 length

let reference = String.make 16 '\255'

let write_flash k ~address data offset length =
  let send_length = 64 and receive_length = 1 in
  (* les écritures se font par blocs de 16 octets *)
  let increment = 16 in
  let send_buffer = String.create send_length in
  let receive_buffer = String.create receive_length in
  let rec loop address offset total_length =
    (* address: sur le PIC, offset: dans data, total_length: taille restante *)
    if total_length <= 0 then
      return ()
    else begin
      let packet = String.make increment '\255' in
      String.blit data offset packet 0 (min total_length increment);
      if packet = reference then begin
        (* le paquet n'a pas de contenu, on l'ignore *)
        (* lwt () = printf "Skipping address 0x%06X...\n" address in *)
        loop (address+increment) (offset+increment) (total_length-increment)
      end else begin
        (* lwt () = printf "Processing address 0x%06X...\n" address in *)
        put_message send_buffer PcInterface.write_flash increment address packet;
        lwt () = send_receive_packet k send_buffer send_length receive_buffer receive_length 0.5 1. in
        if int_of_char receive_buffer.[0] <> PcInterface.write_flash then
          failwith (UnexpectedReply (receive_buffer, String.make 1 (char_of_int PcInterface.erase_flash)))
        else begin
          lwt written = get_flash k ~address ~length:increment in
          if written <> packet then
            failwith (WriteError (packet, written))
          else
            loop (address+increment) (offset+increment) (total_length-increment)
        end
      end
    end
  in loop address offset length

let reset_board k =
  let send_buffer = String.create 64 and receive_buffer = String.create 64 in
  send_buffer.[0] <- char_of_int PcInterface.reset;
  lwt () = send_receive_packet k send_buffer 1 receive_buffer 64 5. 5. in
  return ()
