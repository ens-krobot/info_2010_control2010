(*
 * krobot_daemon.ml
 * ----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

let section = Lwt_log.Section.make "main"

open Lwt
open Krobot_dbus_daemon.Fr_krobot_Daemon

let path = ["fr"; "krobot"; "Daemon"]

let shutdown peer =
    OBus_method.call m_shutdown (OBus_proxy.make peer path) ()

let log peer =
  OBus_signal.make s_log (OBus_proxy.make peer path)

module Server =
struct
  type t = {
    obus : t OBus_object.t;
    mutable quit : bool;
    quit_waiter : unit Lwt.t;
    quit_wakener : unit Lwt.u;
  }

  let log daemon message =
    OBus_signal.emit s_log daemon.obus message

  let shutdown daemon =
    if daemon.quit then begin
      daemon.quit_waiter
    end else begin
      daemon.quit <- true;
      lwt () = Lwt_log.info ~section "shutdown method invoked" in
      wakeup daemon.quit_wakener ();
      exit 0
    end

  let interface = make { m_shutdown = (fun obj () -> shutdown (OBus_object.get obj)) }
end

let start ~desc ~name init =
  let no_fork =
    Krobot_arg.flag_d
      ~section:"Daemon"
      ~key:"-no-fork"
      ~doc:"do not daemonize"
      ~default:false
      ()
  and kill =
    Krobot_arg.flag_d
      ~section:"Daemon"
      ~key:"-kill"
      ~doc:(Printf.sprintf "kill any running %s service and exit" desc)
      ~default:false
      ()
  in
  Krobot_arg.parse ();

  lwt () = Lwt_log.info ~section "openning the robot bus" in
  lwt bus = Krobot_dbus.open_bus () in

  lwt () =
    if Lazy.force kill then begin
      try_lwt
        lwt owner = OBus_bus.get_name_owner bus name in
        lwt () = Lwt_log.info_f ~section "killing the running %s service" desc in
        shutdown (OBus_peer.make bus owner)
      with OBus_bus.Name_has_no_owner _ ->
        return ()
      finally
        exit 0
    end else
      return ()
  in

  (* Exit the program when we lost the name *)
  lwt () =
      Lwt_event.always_notify
        (fun lost_name -> if name = lost_name then exit 0)
    =|< OBus_signal.connect (OBus_bus.name_lost bus)
  in

  lwt () =
    if Lazy.force no_fork then
      Lwt_log.notice_f ~section "starting %s in foreground mode" desc
    else begin
      lwt () = Lwt_log.notice_f ~section "starting %s in daemon mode" desc in
      Lwt_daemon.daemonize ();
      return ()
    end
  in

  let quit_waiter, quit_wakener = wait () in
  let daemon = {
    Server.obus = OBus_object.make ~interfaces:[Server.interface] path;
    Server.quit = false;
    Server.quit_waiter = quit_waiter;
    Server.quit_wakener = quit_wakener;
  } in
  OBus_object.attach daemon.Server.obus daemon;
  OBus_object.export bus daemon.Server.obus;

  let dbus_logger =
    Lwt_log.make
      (fun section level lines ->
         if level > Lwt_log.Info then
           let buf = Buffer.create 42 in
           let lines =
             List.map
               (fun line ->
                  Buffer.clear buf;
                  Lwt_log.render ~buffer:buf ~template:"$(level)@$(name)[$(section)]: $(message)" ~section ~level ~message:line;
                  Buffer.contents buf)
               lines
           in
           Server.log daemon (String.concat "\n" lines)
         else
           return ())
      return
  in

  Lwt_log.default := Lwt_log.broadcast [!Lwt_log.default; dbus_logger];

  (* Does the program specifics initialisation *)
  lwt () = init bus in

  (* Request the name once obejct are exported, to avoid race
     condition *)
  lwt () =
    OBus_bus.request_name bus ~allow_replacement:true ~replace_existing:true name >>= function
      | `Primary_owner ->
          return ()
      | _ ->
          lwt () = Lwt_log.error_f ~section "cannot get the %S name, exiting" name in
          exit 1
  in

  fst (wait ())
