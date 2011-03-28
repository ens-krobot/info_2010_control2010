(*
 * krobot_dynamixel_over_serial.ml
 * -------------------------------
 * Copyright : (c) 2010, Stéphane Glondu <steph@glondu.net>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

let section = Lwt_log.Section.make "rx64-card"

open Lwt

exception Error of string

let broadcast_id = 0xFE

type error =
  | E_Instruction
  | E_Overload
  | E_Checksum
  | E_Range
  | E_Overheating
  | E_AngleLimit
  | E_InputVoltage

type instr =
  | I_PING
  | I_READ_DATA
  | I_WRITE_DATA
  | I_REG_WRITE
  | I_ACTION
  | I_RESET
  | I_SYNC_WRITE

let int_of_instr = function
  | I_PING -> 0x01
  | I_READ_DATA -> 0x02
  | I_WRITE_DATA -> 0x03
  | I_REG_WRITE -> 0x04
  | I_ACTION -> 0x05
  | I_RESET -> 0x06
  | I_SYNC_WRITE -> 0x83

let errors_of_code code =
  let res = ref [] in
  let push mask e =
    if code land mask <> 0 then res := e::!res
  in
  push 0x01 E_InputVoltage;
  push 0x02 E_AngleLimit;
  push 0x04 E_Overheating;
  push 0x08 E_Range;
  push 0x10 E_Checksum;
  push 0x20 E_Overload;
  push 0x40 E_Instruction;
  !res

let error_message errors =
  String.concat "; "
    (List.map
       (function
          | E_Instruction -> "illegal instruction"
          | E_Overload -> "torque overload"
          | E_Checksum -> "invalid checksum"
          | E_Range -> "instruction out of range"
          | E_Overheating -> "overheating"
          | E_AngleLimit -> "angle out of range"
          | E_InputVoltage -> "voltage out of range")
       errors)


(* +-----------------------------------------------------------------+
   | Serialization                                                   |
   +-----------------------------------------------------------------+ *)

(* FIXME: should be factored with USBCard *)

type pointer = {
  mutable offset : int;
  buffer : string;
}

(* Beware: 16 bits integers are little-endian on RX-64, unlike
   USBCards *)

let get_uint8 pointer =
  let offset = pointer.offset in
  pointer.offset <- offset + 1;
  Char.code pointer.buffer.[offset]
let put_uint8 pointer value =
  let offset = pointer.offset in
  pointer.offset <- offset + 1;
  pointer.buffer.[offset] <- Char.unsafe_chr value
let get_sint8 = get_uint8
let put_sint8 = put_uint8

let put_char pointer x =
  let offset = pointer.offset in
  pointer.offset <- offset + 1;
  pointer.buffer.[offset] <- x

let get_sint16 pointer =
  let v1 = get_uint8 pointer in
  let v0 = get_uint8 pointer in
  (v0 lsl 8) lor v1
let get_uint16 = get_sint16

let put_sint16 pointer value =
  put_uint8 pointer (value land 0xff);
  put_uint8 pointer ((value lsr 8) land 0xff)
let put_uint16 = put_sint16

let get_sint32 pointer =
  let v3 = get_uint8 pointer in
  let v2 = get_uint8 pointer in
  let v1 = get_uint8 pointer in
  let v0 = get_uint8 pointer in
  (v0 lsl 24) lor (v1 lsl 16) lor (v2 lsl 8) lor v3
let get_uint32 = get_sint32

let put_sint32 pointer value =
  put_uint8 pointer (value land 0xff);
  put_uint8 pointer ((value lsr 8) land 0xff);
  put_uint8 pointer ((value lsr 16) land 0xff);
  put_uint8 pointer ((value lsr 24) land 0xff)
let put_uint32 = put_sint32

let get_string pointer =
  let index =
    try
      String.index_from pointer.buffer pointer.offset '\000'
    with Not_found ->
      String.length pointer.buffer
  in
  let offset = pointer.offset in
  pointer.offset <- index + 1;
  String.sub pointer.buffer offset (index - offset)

let put_string pointer value =
  let len = String.length value in
  if len > String.length pointer.buffer - pointer.offset then
    invalid_arg "RW.put_string: string too long"
  else begin
    String.blit value 0 pointer.buffer pointer.offset len;
    let offset = pointer.offset + len in
    if offset < String.length pointer.buffer then begin
      pointer.buffer.[offset] <- '\x00';
      pointer.offset <- offset + 1
    end else
      pointer.offset <- offset
  end

(* +-----------------------------------------------------------------+
   | Messages                                                        |
   +-----------------------------------------------------------------+ *)

(** Instruction packet, sent from controller to actuator *)
type instruction = {
  i_id : int;
  i_instr : int;
  i_data : string;
}

(** Status packet, sent from actuator to controller *)
type status = {
  s_id : int;
  s_errors : error list;
  s_data : string;
}

let compute_checksum buf ofs len =
  let rec aux i accu =
    if i < ofs then (lnot accu) land 0xFF
    else aux (i-1) (accu + int_of_char buf.[i])
  in aux (ofs+len-1) 0

(** Parse a message from a raw buffer *)
let parse_message buf =
  let n = String.length buf in
  assert (5 <= n);
  if buf.[0] <> '\255' || buf.[1] <> '\255' then
    `Error "invalid header"
  else
    let length = int_of_char buf.[3] in
    let size = length+4 in
    if n < size then
      Printf.ksprintf
        (fun x -> `Error x)
        "incohent size in message (%d/%d)"
        size n
    else
      let checksum = compute_checksum buf 2 (length+1) in
      if int_of_char buf.[size-1] <> checksum then begin
        Printf.ksprintf
          (fun s -> `Error s)
          "bad checksum in %S"
          (String.sub buf 0 size)
      end else
        `Some {
          s_id = int_of_char buf.[2];
          s_errors = errors_of_code (int_of_char buf.[4]);
          s_data = String.sub buf 5 (length-1);
        }

(** Format an instruction message *)
let format_message msg =
  let n = String.length msg.i_data in
  let res = String.create (n+6) in
  let ptr = { offset = 0; buffer = res } in
  (* FIXME: handle possible errors of char_of_int *)
  put_uint8 ptr 0xFF;
  put_uint8 ptr 0xFF;
  put_uint8 ptr msg.i_id;
  put_uint8 ptr (n+2);
  put_uint8 ptr msg.i_instr;
  String.iter (put_char ptr) msg.i_data;
  put_uint8 ptr (compute_checksum res 2 (n+3));
  assert (ptr.offset = n+6);
  res

(* +-----------------------------------------------------------------+
   | Definitions                                                     |
   +-----------------------------------------------------------------+ *)

exception Card_closed
exception Card_crashed of string

(* Type of a up and running card *)
type card = {

  handle : Lwt_unix.file_descr;
  (* Handle pour le périphérique série *)

  mutex : Lwt_mutex.t;
  (* Mutex pour envoyer des commandes, les cartes n'aiment pas les
     appels parallèles. *)

  abort_waiter : int Lwt.t;
  abort_wakener : int Lwt.u;
  (* Sleeping thread which is wakeup when the card is closed *)

  wrapper : wrapper;
  (* The associated wrapper *)

  timeout : float;
}

and state =
  | Opened of card
  | Closed of exn

and wrapper = {
  mutable state : state;
  name : string;
  watch : [ `Error of exn | `Closed ] Lwt.t;
}

let closed wrapper = match wrapper.state with
  | Opened _ -> false
  | Closed _ -> true

let watch wrapper = wrapper.watch

(* Return a running card, if possible. *)
let get_card wrapper = match wrapper.state with
  | Opened card ->
      return card
  | Closed exn ->
      raise_lwt exn

(* +-----------------------------------------------------------------+
   | Aborting                                                        |
   +-----------------------------------------------------------------+ *)

let abort wrapper exn =
  match wrapper.state with
    | Closed exn ->
        return exn
    | Opened card ->
        wrapper.state <- Closed exn;
        try_lwt
          lwt () = Lwt_unix.close card.handle in
          return exn
        finally
          wakeup_exn card.abort_wakener exn;
          return ()

(* +-----------------------------------------------------------------+
   | Opening and closing                                             |
   +-----------------------------------------------------------------+ *)

let close_wrapper wrapper = match wrapper.state with
  | Opened _ ->
      lwt _ = abort wrapper Card_closed in
      return ()
  | Closed _ ->
      return ()

let make_wrapper ?(timeout=1.0) ~name ~path ~rate =
  lwt fd = Lwt_unix.openfile path [Unix.O_RDWR; Unix.O_NONBLOCK] 0o660 in
  let tio = {
    (* taken from minicom *)
    Unix.c_ignbrk = true; Unix.c_brkint = false; Unix.c_ignpar = false;
    Unix.c_parmrk = false; Unix.c_inpck = false; Unix.c_istrip = false;
    Unix.c_inlcr = false; Unix.c_igncr = false; Unix.c_icrnl = false;
    Unix.c_ixon = false; Unix.c_ixoff = false; Unix.c_opost = false;
    Unix.c_obaud = rate; Unix.c_ibaud = rate; Unix.c_csize = 8;
    Unix.c_cstopb = 1; Unix.c_cread = true; Unix.c_parenb = false;
    Unix.c_parodd = false; Unix.c_hupcl = false; Unix.c_clocal = true;
    Unix.c_isig = false; Unix.c_icanon = false; Unix.c_noflsh = false;
    Unix.c_echo = false; Unix.c_echoe = false; Unix.c_echok = false;
    Unix.c_echonl = false; Unix.c_vintr = '\000'; Unix.c_vquit = '\000';
    Unix.c_verase = '\000'; Unix.c_vkill = '\000'; Unix.c_veof = '\000';
    Unix.c_veol = '\000'; Unix.c_vmin = 1; Unix.c_vtime = 5;
    Unix.c_vstart = '\000'; Unix.c_vstop = '\000'
  } in
  lwt () = Lwt_unix.tcsetattr fd Unix.TCSANOW tio in
  let abort_waiter, abort_wakener = wait () in
  let rec card = {
    handle = fd;
    mutex = Lwt_mutex.create ();
    abort_waiter = abort_waiter;
    abort_wakener = abort_wakener;
    wrapper = wrapper;
    timeout = timeout;
  } and wrapper = {
    state = Opened card;
    name = name;
    watch = (try_lwt
               lwt _ = abort_waiter in
               (* Never happen: *)
               return `Closed
             with
               | Card_closed ->
                   return `Closed
               | exn ->
                   return (`Error exn))
  } in
  return wrapper

(** Wrap [f] into proper ioctls to read from the card *)
let with_emit_mode f card =
  let fd = Lwt_unix.unix_file_descr card.handle in
  let r = Serial.tiocmbic fd 0x004 in
  if r <> 0 then
    Printf.ksprintf
      (fun s -> raise_lwt (Failure s))
      "TIOCMBIC(TIOCM_RTS) failed with code %d"
      r
  else
    try_lwt
      f card
    finally
      let r = Serial.tiocmbis fd 0x004 in
      if r <> 0 then
        let msg = Printf.sprintf "TIOCMBIS(TIOCM_RTS) failed with error %d (ignored)" r in
        Lwt_log.error ~section msg
      else
        return ()

(* +-----------------------------------------------------------------+
   | Sending/receiving messages                                      |
   +-----------------------------------------------------------------+ *)



(** Read and parse a message from serial link *)
let read_message =
  (* messages are theoretically <= 260 bytes *)
  let buf = String.create 512 in
  with_emit_mode
  begin fun card ->
    lwt n = pick [
      (Lwt_unix.sleep card.timeout >> return (-1));
      Lwt_unix.read card.handle buf 0 4
    ] in
    if n < 0 then
      return (`Error "no response")
    else if n < 4 then
      return (`Error "unable to read message header")
    else if buf.[0] <> '\255' || buf.[1] <> '\255' then
      Printf.ksprintf
        (fun s -> return (`Error s))
        "bad header (starting with [%02X %02X])"
        (int_of_char buf.[0]) (int_of_char buf.[1])
    else
      let length = int_of_char buf.[3] in
      let rec loop_read index remaining total =
        lwt n = Lwt.pick [
          (Lwt_unix.sleep card.timeout >> return 0);
          Lwt_unix.read card.handle buf index remaining;
        ] in
        if n <= 0 then
          return total
        else if n < remaining then
          loop_read (index+n) (remaining-n) (total+n)
        else
          return (total+n)
      in
      lwt n = loop_read 4 length 0 in
      if n <> length then
        Printf.ksprintf
          (fun s -> return (`Error s))
          "unable to read the whole message (%d/%d byte(s) read)"
          n length
      else
        return (parse_message buf)
 end

let send card buffer =
  let n = String.length buffer in
  lwt len = pick [
    card.abort_waiter;
    Lwt_unix.write card.handle buffer 0 n
  ] in
  if len <> n then
    return (`WriteError (len, n))
  else
    read_message card

let call wrapper id instr data =
  lwt card = get_card wrapper in
  let buffer = format_message {
    i_id = id;
    i_instr = int_of_instr instr;
    i_data = data;
  } in
  Lwt_mutex.with_lock card.mutex
    (fun () ->
       lwt r = send card buffer in
       match r with
         | `WriteError (a, b) ->
             Printf.ksprintf
               (fun s -> raise_lwt (Error s))
               "%d byte(s) written instead of %d"
               a b
         | `Error s ->
             raise_lwt (Error s)
         | `Some status ->
             if status.s_errors = [] then
               return status.s_data
             else
               raise_lwt (Error(error_message status.s_errors)))

let () = Printexc.register_printer
  (function
     | Error e -> Some e
     | _ -> None)

(* +-----------------------------------------------------------------+
   | The device class                                                |
   +-----------------------------------------------------------------+ *)

type t = {
  mutable wrapper : wrapper option;
  mutable closed : bool;
  name : string;
  state : [ `Up | `Down ] React.signal;
  set_state : [`Up | `Down ] -> unit;
}

let unavailable card =
  raise_lwt
    (Krobot_error.Device.Unavailable
       (Printf.sprintf
          "the %s card is currently not available"
          card.name))

let name card = card.name
let state card = card.state

let close card =
  card.closed <- true;
  let wrapper = card.wrapper in
  card.wrapper <- None;
  card.set_state `Down;
  match wrapper with
    | Some wrapper ->
        close_wrapper wrapper
    | None ->
        return ()

let ping card ~id =
  match card.wrapper with
    | None ->
        unavailable card
    | Some wrapper ->
        lwt _ = call wrapper id I_PING "" in
        return ()

let read card ~id ~address ~length =
  match card.wrapper with
    | None ->
        unavailable card
    | Some wrapper ->
        let data = String.create 2 in
        data.[0] <- char_of_int address;
        data.[1] <- char_of_int length;
        lwt data = call wrapper id I_READ_DATA data in
        return (String.sub data 0 2)

let write card ~id ~address ~data =
  match card.wrapper with
    | None ->
        unavailable card
    | Some wrapper ->
        let data = String.make 1 (char_of_int address) ^ data in
        lwt _ = call wrapper id I_WRITE_DATA data in
        return ()

let reset card ~id =
  match card.wrapper with
    | None ->
        unavailable card
    | Some wrapper ->
        lwt _ = call wrapper id I_RESET "" in
        return ()

let make ?timeout ~name ~path ~rate () =
  let state, set_state = React.S.create `Down in
  let card = {
    wrapper = None;
    closed = false;
    name = name;
    state = state;
    set_state = set_state;
  } in
  let rec loop () =
    lwt () =
      try_lwt
        lwt wrapper = make_wrapper ?timeout ~name ~path ~rate in
        card.wrapper <- Some wrapper;
        card.set_state `Up;
        lwt _ = wrapper.watch in
        return ()
      with exn ->
        return ()
    in
    if card.closed then
      return ()
    else
      lwt () = Lwt_unix.sleep 0.1 in
      loop ()
  in
  ignore (loop ());
  card

