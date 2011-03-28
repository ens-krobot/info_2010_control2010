(*
 * krobot_usb.ml
 * -------------
 * Copyright : (c) 2009-2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

let section = Lwt_log.Section.make "usb-card"

open Lwt
open Krobot_device

let error_messages = [
  PcInterface.err_unknown_cmd, "unknown command";
  PcInterface.err_unknown_get, "unknown get request";
  PcInterface.err_unknown_set, "unknown set request";
  PcInterface.err_invalid_response, "invalid response";
  PcInterface.err_ax12_wrong_packet, "invalid AX12 packet";
  PcInterface.err_ax12_error, "AX12 error";
  PcInterface.err_ax12_chksum, "invalid checksum of AX12 packet";
  PcInterface.err_cmp03_not_responding, "cmp03 not responding";
  PcInterface.err_adjd_s371_not_responding, "adjd_s371 not responding";
  PcInterface.err_lm_command_error, "lm command error";
  PcInterface.err_lm_position_error, "lm position error";
  PcInterface.err_invalid_axis, "invalid axis";
]

let error_message error =
  try
    List.assoc error error_messages
  with Not_found ->
    Printf.sprintf "unknown error (%d)" error

(* +-----------------------------------------------------------------+
   | Messages                                                        |
   +-----------------------------------------------------------------+ *)

let data_length = 52
  (* Taille en octet du corps d'un message *)

type serial = int
    (* Type d'un numéro de série d'un message *)

type message = {
  host_serial : serial;
  (* Le numéro de série du message, émis par l'ordinateur. Vaut 0 pour
     les messages émis par le PIC. *)

  device_serial : serial;
  (* Le numéro de série du message, émis par le PIC. Vaut 0 pour les
     messages émis par l'ordinateur. *)

  command : int;
  (* La commande, en fait c'est plutôt le type du message *)

  error : int;
  (* Si c'est un message d'erreur ce flag est non-nul *)

  data : string;
  (* Les données du messages, il y a 52 octets. *)
}

let make_buffer () = String.make data_length '\000'

(* Parse un message depuis un buffer brut: *)
let parse_message buf = {
  host_serial = Char.code buf.[PcInterface.up_hseq];
  device_serial = Char.code buf.[PcInterface.up_dseq];
  command = Char.code buf.[PcInterface.up_cmd];
  error = Char.code buf.[PcInterface.up_err];
  data = String.sub buf PcInterface.up_data 52;
}

(* Crée un buffer brut depuis un message: *)
let forge_message msg =
  let buf = String.make 64 '\000' in
  buf.[PcInterface.up_hseq] <- Char.chr msg.host_serial;
  buf.[PcInterface.up_dseq] <- Char.chr msg.device_serial;
  buf.[PcInterface.up_cmd] <- Char.chr msg.command;
  buf.[PcInterface.up_err] <- Char.chr msg.error;
  if String.length msg.data > 52 then
    Printf.ksprintf invalid_arg "message body too big: %d > 52" (String.length msg.data)
  else begin
    String.blit msg.data 0 buf PcInterface.up_data (String.length msg.data);
    buf
  end

(* +-----------------------------------------------------------------+
   | The USB devie type                                              |
   +-----------------------------------------------------------------+ *)

type t = {
  name : string;
  (* The name of the card *)

  vendor_id : int;
  product_id : int;

  state : [ `Up | `Down ] React.signal;
  set_state : [ `Up | `Down ] -> unit;
  (* Current state of the card *)

  errors : string React.event;
  send_error : string -> unit;
  (* Event which occurs when the card send an error *)

  commands : (int * string) React.event;
  send_command : (int * string) -> unit;
  (* Event which occurs when the card send a spontaneous command *)

  reply_waiters : string Lwt.u option array;
  (* Threads waiting for a reply *)

  mutable handle : USB.handle option;
  (* The current USB handle *)

  mutable kernel_driver_active : bool;
  (* Whether a kernel driver was attached to the peripherial before we
     opened it *)

  mutex : Lwt_mutex.t;
  (* Mutex for sending commands *)

  mutable abort_waiter : int Lwt.t;
  mutable abort_wakener : int Lwt.u;
  (* Sleeping thread which is wakeup when the card is reopened or
     closed *)

  mutable closed : bool;
  (* Initially [false], and set to [true] when the card is closed *)
}

(* +-----------------------------------------------------------------+
   | Projections                                                     |
   +-----------------------------------------------------------------+ *)

let errors card = card.errors
let commands card = card.commands
let state card = card.state
let name card = card.name

(* +-----------------------------------------------------------------+
   | Serials                                                         |
   +-----------------------------------------------------------------+ *)

let serial_count = 256

let get_serial card =
  let rec loop n =
    if n = serial_count then
      raise (Krobot_error.Device.Error "no more serial available!")
    else match card.reply_waiters.(n) with
      | Some _ -> loop (n + 1)
      | None -> n
  in
  loop 0

(* +-----------------------------------------------------------------+
   | Sending/receiving messages                                      |
   +-----------------------------------------------------------------+ *)

let abort card exn =
  if Lwt.state card.abort_waiter = Sleep then
    wakeup_exn card.abort_wakener exn

let send card buffer =
  match card.handle with
    | None ->
        raise_lwt
          (Krobot_error.Device.Unavailable
             (Printf.sprintf
                "the %s card is currently not available"
                card.name))
    | Some handle ->
        lwt len = pick [card.abort_waiter; USB.interrupt_send ~handle ~timeout:1.0 ~endpoint:1 buffer 0 64] in
        if len <> 64 then begin
          let exn =
            Krobot_error.Device.Error
              (Printf.sprintf
                 "write on %s card returned %d instead of 64, reopening"
                 card.name len)
          in
          abort card exn;
          raise_lwt exn
        end else
          return ()

(* Send a command without waiting for the reply: *)
let call_no_reply card ~command ~i_types args =
  let buffer = forge_message {
    host_serial = 0;
    device_serial = 0;
    command = command;
    error = 0;
    data = Krobot_wire.write i_types args;
  } in
  try_lwt
    Lwt_mutex.with_lock card.mutex
      (fun () ->
         lwt () = send card buffer in
         return ())
  with
    | Canceled ->
        raise_lwt Canceled
    | exn  ->
        abort card exn;
        raise_lwt exn

(* Send a command and wait for the reply: *)
let call card ?(timeout=1.0) ~command ~i_types ~o_types args =
  let waiter, wakener = Lwt.task () in
  let serial = get_serial card in
  card.reply_waiters.(serial) <- Some wakener;
  on_cancel waiter (fun () -> card.reply_waiters.(serial) <- None);
  let buffer = forge_message {
    host_serial = serial;
    device_serial = 0;
    command = command;
    error = 0;
    data = Krobot_wire.write i_types args;
  } in
  try_lwt
    Lwt_mutex.with_lock card.mutex
      (fun () ->
         lwt () = send card buffer in
         lwt buffer = Lwt_unix.with_timeout timeout (fun () -> waiter) in
         return (Krobot_wire.read o_types buffer))
  with
    | Canceled ->
        raise_lwt Canceled
    | exn  ->
        abort card exn;
        raise_lwt exn

(* +-----------------------------------------------------------------+
   | Logging                                                         |
   +-----------------------------------------------------------------+ *)

(* Log dropped messages *)
let dropped card typ msg =
  lwt data = Lwt_stream.to_list (Lwt_stream.hexdump (Lwt_stream.of_string msg.data)) in
  Lwt_log.warning_f ~section "%s dropped on card %s
                               \  host_serial = %d\n\
                               \  device_serial = %d\n\
                               \  command = %d\n\
                               \  error = %s\n\
                               \  data:\n%s"
    typ card.name msg.host_serial msg.device_serial msg.command
    (if msg.error <> 0 then error_message msg.error else "none")
    (String.concat "" (List.map (Printf.sprintf "    %s\n") data))

(* +-----------------------------------------------------------------+
   | Dispatching                                                     |
   +-----------------------------------------------------------------+ *)

(* Dispatch one message *)
let dispatch card msg =
  if msg.error <> 0 then begin
    if msg.command = PcInterface.cmd_respond then
      card.send_error ("response: " ^ error_message msg.error)
    else
      card.send_error ("spontaneous: " ^ error_message msg.error)
  end;
  if msg.command = PcInterface.cmd_respond then begin
    match card.reply_waiters.(msg.host_serial) with
      | Some wakener ->
          card.reply_waiters.(msg.host_serial) <- None;
          if msg.error <> 0 then
            wakeup_exn wakener (Krobot_error.Device.Error(error_message msg.error))
          else
            wakeup wakener msg.data;
          return ()
      | None ->
          dropped card "response" msg
  end else begin
    try
      lwt () = Lwt_log.debug_f ~section "command %d received" msg.command in
      card.send_command (msg.command, msg.data);
      return ()
    with exn ->
      Lwt_log.error_f ~section ~exn "pushing event %d from %s card failed with" msg.command card.name
  end

(* +-----------------------------------------------------------------+
   | Closing the card                                                |
   +-----------------------------------------------------------------+ *)

let cleanup_no_usb exn card =
  card.handle <- None;
  card.set_state `Down;
  let abort_waiter = card.abort_waiter and abort_wakener = card.abort_wakener in
  let waiter, wakener = wait () in
  card.abort_waiter <- waiter;
  card.abort_wakener <- wakener;
  if Lwt.state abort_waiter = Sleep then wakeup_exn abort_wakener exn;
  for i = 0 to serial_count - 1 do
    match card.reply_waiters.(i) with
      | Some wakener ->
          card.reply_waiters.(i) <- None;
          wakeup_exn wakener exn
      | None ->
          ()
  done

let cleanup exn card =
  match card.handle with
    | None ->
        return ()
    | Some handle ->
        cleanup_no_usb exn card;
        try_lwt
          lwt () = USB.release_interface handle 0 in
          if card.kernel_driver_active then USB.attach_kernel_driver handle 0;
          return ()
        with exn ->
          Lwt_log.error_f ~section ~exn "failed to close the %s card" card.name
        finally
          USB.close handle;
          return ()

let close card =
  card.closed <- true;
  cleanup (Krobot_error.Device.Error(Printf.sprintf "the %s card has been closed" card.name)) card

let safe_cleanup exn card =
  try_lwt
    cleanup exn card
  with exn ->
    Lwt_log.error_f ~section ~exn "failed to release card %s" card.name

(* +-----------------------------------------------------------------+
   | Device monitoring                                               |
   +-----------------------------------------------------------------+ *)

exception Reopen_card

let rec loop card =
  if card.closed then
    return ()
  else
    lwt () = Lwt_log.info_f ~section "opening card %s" card.name in
    try_lwt
      let handle = try USB.open_device_with ~vendor_id:card.vendor_id ~product_id:card.product_id with Failure _ -> raise Reopen_card in
      card.handle <- Some handle;
      lwt () = Lwt_log.info_f ~section "card %s opened" card.name in

      (* USB stuff *)
      card.kernel_driver_active <- USB.kernel_driver_active handle 0;
      if card.kernel_driver_active then USB.detach_kernel_driver handle 0;
      lwt () = USB.claim_interface handle 0 in
      lwt () = Lwt_log.info_f ~section "card %s is ready" card.name in

      (* Everything is ready, set the card up *)
      card.set_state `Up;

      let rec read_and_dispatch () =
        let buffer = String.create 64 in
        lwt len = pick [card.abort_waiter; USB.interrupt_recv ~handle ~endpoint:1 buffer 0 64] in
        lwt () =
          if len <> 64 then
            lwt () = Lwt_log.warning_f ~section "read on %s card returned %d instead of 64, reopening" card.name len in
            raise_lwt Reopen_card
          else
            return ()
        in
        ignore (dispatch card (parse_message buffer));
        read_and_dispatch ()
      in
      read_and_dispatch ()
    with
      | _ when card.closed ->
          return ()
      | Reopen_card ->
          lwt () = safe_cleanup Reopen_card card in
          lwt () = Lwt_unix.sleep 0.1 in
          loop card
      | exn ->
          lwt () = Lwt_log.error_f ~section ~exn  "error on card %s" card.name in
          cleanup_no_usb exn card;
          lwt () = Lwt_unix.sleep 0.1 in
          loop card

(* +-----------------------------------------------------------------+
   | Card creation                                                   |
   +-----------------------------------------------------------------+ *)

let make ~name ~vendor_id ~product_id () =
  let state, set_state = React.S.create `Down
  and errors, send_error = React.E.create ()
  and commands, send_command = React.E.create ()
  and abort_waiter, abort_wakener = Lwt.wait () in
  let card = {
    name = name;
    vendor_id = vendor_id;
    product_id = product_id;
    state = state;
    set_state = set_state;
    errors = errors;
    send_error = send_error;
    commands = commands;
    send_command = send_command;
    reply_waiters = Array.make serial_count None;
    handle = None;
    kernel_driver_active = false;
    mutex = Lwt_mutex.create ();
    abort_waiter = abort_waiter;
    abort_wakener = abort_wakener;
    closed = false;
  } in
  ignore (loop card);
  card
