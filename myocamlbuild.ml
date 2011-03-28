(*
 * myocamlbuild.ml
 * ---------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Printf
open Ocamlbuild_plugin

(* +-----------------------------------------------------------------+
   | Configuration                                                   |
   +-----------------------------------------------------------------+ *)

let try_exec command =
  try
    let () = Command.execute ~quiet:true (Cmd(S[Sh command; Sh"> /dev/null"; Sh"2> /dev/null"])) in
    true
  with _ ->
    false

let () =
  if not (try_exec "ocamlfind printconf") then begin
    prerr_endline "ocamlfind is not available, please install it";
    exit 1
  end

let have_native = try_exec "ocamlfind ocamlopt -version"
let have_sdl = try_exec "ocamlfind query sdl"
let have_obus = try_exec "ocamlfind query obus"
let have_lwt_text = try_exec "ocamlfind query lwt.text"
let have_usb = try_exec "ocamlfind query usb"
let have_serial = try_exec "ocamlfind query serial"
let have_lwt_glib = try_exec "ocamlfind query lwt.glib"
let have_lablgtk2 = try_exec "ocamlfind query lablgtk2"
let have_cairo = try_exec "ocamlfind query cairo"

let () =
  let yes_no = function true -> "yes" | false -> "no" in
  printf "\
+--[ compilation options ]----------+
| native compilation:           %3s |
| sdl (joystick):               %3s |
| obus:                         %3s |
| lwt.text:                     %3s |
| usb:                          %3s |
| serial:                       %3s |
+-----------------------------------+
%!" (yes_no have_native)
    (yes_no have_sdl)
    (yes_no have_obus)
    (yes_no have_lwt_text)
    (yes_no have_usb)
    (yes_no have_serial)

let () =
  if not have_obus || not have_lwt_text then begin
    prerr_endline "required depencies obus and lwt.text are not installed, cannot compile";
    exit 1
  end

let targets = List.filter_opt (function (true, target) -> Some target | (false, target) -> None) [
  (* Driver *)
  (have_usb && have_serial, "driver/krobot_driver.best");

  (* Card tools *)
  (have_usb, "usb-tools/krobot_send_firmware.best");
  (have_usb, "usb-tools/krobot_dump_memory.best");

  (* Krobot client library *)
  (true, "krobot.cma");
  (have_native, "krobot.cmxa");
  (have_native, "krobot.cmxs");

  (* Tools *)
  (true, "tools/krobot_forward_dbus.best");

  (* Clients *)
  (true, "clients/krobot_servos_control.best");
  (true, "clients/krobot_jack.best");
  (true, "clients/krobot_joystick.best");
  (true, "clients/krobot_recorder.best");
  (true, "clients/krobot_record_infrared.best");
  (true, "clients/krobot_write_lcd.best");
  (true, "clients/krobot_controller.best");

  (* Commands *)
  (true, "clients/commands/krobot_move.best");
  (true, "clients/commands/krobot_turn.best");

  (* Services *)
  (true, "services/krobot_service_claws.best");
  (true, "services/krobot_service_motors.best");
  (true, "services/krobot_service_gate.best");
  (true, "services/krobot_service_sensors.best");
  (true, "services/krobot_service_grip.best");
  (true, "services/krobot_service_turret.best");
  (true, "services/stoppers/krobot_range_finders_stop.best");
  (true, "services/stoppers/krobot_infrareds_stop.best");

  (* Python *)
  (true, "python/generate.best");

  (* Simulator *)
  (have_cairo && have_lablgtk2 && have_lwt_glib, "simulator/krobot_simulator.best");
]

(* +-----------------------------------------------------------------+
   | Utils                                                           |
   +-----------------------------------------------------------------+ *)

let flag_all_stages_except_link tag f =
  flag ["ocaml"; "compile"; tag] f;
  flag ["ocaml"; "ocamldep"; tag] f;
  flag ["ocaml"; "doc"; tag] f

let flag_all_stages tag f =
  flag_all_stages_except_link tag f;
  flag ["ocaml"; "link"; tag] f

let _ =
  dispatch begin function
    | Before_options ->
        Options.use_ocamlfind := true;
        Options.make_links := false

    | After_rules ->

        rule "shared libraries (cmxs)"
          ~dep:"%.cmxa" ~prod:"%.cmxs"
          (fun env _ -> Cmd(S[!(Options.ocamlopt); A"-shared"; A"-linkall"; A(env "%.cmxa"); A"-o"; A(env "%.cmxs")]));

        (* +---------------------------------------------------------+
           | Contexts                                                |
           +---------------------------------------------------------+ *)

        Pathname.define_context "driver" ["protocol"; "common"];
        Pathname.define_context "simulator" ["protocol"; "common"];
        Pathname.define_context "manager" ["protocol"; "common"; "lib-krobot"];
        Pathname.define_context "common" ["protocol"];
        Pathname.define_context "usb-tools" ["common"; "driver"];
        Pathname.define_context "lib-krobot" ["protocol"; "common"];
        Pathname.define_context "clients" ["protocol"; "common"; "lib-krobot"];
        Pathname.define_context "clients/commands" ["protocol"; "common"; "lib-krobot"];
        Pathname.define_context "services" ["protocol"; "common"; "lib-krobot"];
        Pathname.define_context "services/stoppers" ["protocol"; "common"; "lib-krobot"];
        Pathname.define_context "tests" ["protocol"; "common"; "lib-krobot"];

        (* +---------------------------------------------------------+
           | OBus stuff                                              |
           +---------------------------------------------------------+ *)

        rule "D-Bus interface generation: .xml -> .ml, .mli"
          ~dep:"%.xml" ~prods:["%.ml"; "%.mli"]
          (fun env _ -> Cmd(S[A"obus-gen-interface"; A"-o"; A(env "%"); A(env "%.xml")]));

        rule "D-Bus interface generation: .obus -> .ml, .mli"
          ~dep:"%.obus" ~prods:["%.ml"; "%.mli"]
          (fun env _ -> Cmd(S[A"obus-gen-interface"; A"-o"; A(env "%"); A(env "%.obus")]));

        (* +---------------------------------------------------------+
           | Glade                                                   |
           +---------------------------------------------------------+ *)

        rule ".glade -> .ml" ~dep:"%.glade" ~prod:"%.ml"
          (fun env _ ->
             Cmd(S[A"lablgladecc2"; A"-embed"; A(env "%.glade"); Sh">"; A(env "%.ml")]));

        (* +---------------------------------------------------------+
           | Autogenerated files                                     |
           +---------------------------------------------------------+ *)

        (* Geenrate the caml implementation file PcInteface.ml from
           PcInterface.h. It replace all [#define] by let-bindings. *)
        rule "krobot's protocol" ~dep:"driver/PcInterface.h" ~prod:"driver/PcInterface.ml"
          (fun _ _ ->
             Cmd(Sh"awk '$1 == \"#define\" && $3 != \"\" { print \"let \" tolower($2) \" = \" $3 }' driver/PcInterface.h > driver/PcInterface.ml"));

        (* +---------------------------------------------------------+
           | Virtual targets                                         |
           +---------------------------------------------------------+ *)

        if have_native then
          rule "best" ~dep:"%.native" ~prod:"%.best"
            (fun env _ -> ln_s (Filename.basename (env "%.native")) (env "%.best"))
        else
          rule "best" ~dep:"%.byte" ~prod:"%.best"
            (fun env _ -> ln_s (Filename.basename (env "%.byte")) (env "%.best"));

        let virtual_rule name deps =
          rule name ~stamp:name ~deps (fun _ _ -> Nop)
        in

        virtual_rule "all" targets
    | _ -> ()
  end
