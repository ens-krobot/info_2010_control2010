(*
 * krobot_hexfile.ml
 * -----------------
 * Copyright : (c) 2009, Stéphane Glondu <steph@glondu.net>
 * Licence   : BSD3
 *
 * This file is a part of Krobot.
 *)

open Lwt
open Lwt_io

type hex_record =
  | Data of int * string
  | ExtendedLinearAddress of int
  | EndOfFile of int

let string_of_hexline str =
  let n = String.length str in
  assert (n > 0 && n mod 2 = 1 && str.[0] = ':');
  let m = n/2 in
  let result = String.create m in
  for i = 0 to m-1 do
    let j = 2*i+1 in
    result.[i] <- char_of_int (int_of_string ("0x"^(String.sub str j 2)))
  done;
  result

let compute_checksum str =
  let rec aux i accu =
    if i < 0 then
      (-accu) land 0xff
    else
      aux (i-1) (accu+(int_of_char str.[i]))
  in aux (String.length str - 2) 0

let parse_line str =
  let str = string_of_hexline str in
  let get i = int_of_char str.[i] in
  let n = String.length str in
  assert (n >= 5 && compute_checksum str = int_of_char str.[n-1]);
  let count = get 0 in
  assert (count+5 = n);
  let address = ((get 1) lsl 8) lor (get 2) in
  let record_type = get 3 in
  let data = String.sub str 4 count in
  match record_type with
    | 0x00 ->
        Data (address, data)
    | 0x01 ->
        assert (count = 0);
        EndOfFile address
    | 0x04 ->
        assert (count = 2 && address = 0);
        let msb = int_of_char data.[0] and lsb = int_of_char data.[1] in
        (* check for possible overflow *)
        assert (msb land 0x80 = 0);
        ExtendedLinearAddress ((msb lsl 8) lor lsb)
    | _ -> assert false

let parse_file file =
  lwt ic = Lwt_io.open_file ~mode:input file in
  let lines = Lwt_io.read_lines ic in
  let lines = Lwt_stream.map parse_line lines in
  lwt lines = Lwt_stream.get_while (fun _ -> true) lines in
  lwt _ = Lwt_io.close ic in
  return lines

let print_record = function
  | Data (address, data) ->
      Printf.printf "DAT %04x" address;
      String.iter (fun c -> Printf.printf " %02x" (int_of_char c)) data;
      Printf.printf "\n"
  | ExtendedLinearAddress address ->
      Printf.printf "ELA %04x\n" address
  | EndOfFile address ->
      Printf.printf "EOF %04x\n" address

let validate_and_copy hex addr_base buffer offset length =
  assert (offset+length <= String.length buffer);
  let min_address = addr_base+offset in
  let max_address = min_address+length in
  let addr_high = ref 0 in
  let execute_record = function
    | Data (address, data) ->
        assert (address land 0xFFFF = address);
        let address = !addr_high lor address in
        if address < min_address || address >= max_address then
          Printf.eprintf
            "0x%04x is outside range, all bytes dropped\n"
            address
        else begin
          let length =
            let n = String.length data in
            if address+n >= max_address then begin
              Printf.eprintf
                "some bytes at address 0x%04x are outside range (dropped)\n"
                address;
              max_address-address
            end else n
          in
          let offset2 = address-addr_base in
          String.blit data 0 buffer offset2 length;
        end
    | ExtendedLinearAddress address ->
        assert (address land 0x8000 = 0);
        addr_high := address lsl 16
    | EndOfFile address ->
        assert (address = 0);
        raise Exit
  in
  try List.iter execute_record hex with Exit -> ()
