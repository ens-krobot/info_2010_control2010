(*
 * controller.ml
 * -------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

open Lwt
open Lwt_term
open Lwt_read_line

module TextSet = Set.Make(Text)

(* +-----------------------------------------------------------------+
   | Logging                                                         |
   +-----------------------------------------------------------------+ *)

(* Maximum number of lines to keep in logs: *)
let log_count = 16384

(* Signal holding the current logs: *)
let logs, set_logs = React.S.create []

let add_date line =
  let buffer = Buffer.create 42 in
  Lwt_log.render ~buffer ~level:Lwt_log.Info ~message:"" ~template:"$(date): " ~section:Lwt_log.Section.main;
  text (Buffer.contents buffer) :: line

(* Add a list of lines to logs *)
let log_add_lines lines =
  let rec truncate n = function
    | [] ->
        []
    | line :: rest ->
        if n = log_count then
          []
        else
          line :: truncate (n + 1) rest
  in
  set_logs (truncate 0 (List.rev_map add_date lines @ (React.S.value logs)))

let log_add_line line =
  log_add_lines [line]

(* Redirect stderr to logs *)
let redirect_stderr () =
  let rec copy_logs ic =
    lwt line = Lwt_io.read_line ic in
    log_add_line [text line];
    copy_logs ic
  in
  let fdr, fdw = Unix.pipe () in
  Unix.dup2 fdw Unix.stderr;
  Unix.close fdw;
  ignore (copy_logs (Lwt_io.of_unix_fd ~mode:Lwt_io.input fdr))

(* Make the default logger to logs into the log buffer *)
let init_logger () =
  Lwt_log.default :=
    Lwt_log.make
      ~output:(fun section level lines ->
                 log_add_lines
                   (List.map
                      (fun line ->
                         if level >= Lwt_log.Warning then
                           (* Colorize error in red: *)
                           [fg lred; text line]
                         else
                           [text line])
                      lines);
                 return ())
      ~close:return

(* +-----------------------------------------------------------------+
   | Read-line                                                       |
   +-----------------------------------------------------------------+ *)

let engine_state, set_engine_state = React.S.create (Engine.init [])

let history_file_name =
  Filename.concat (try Unix.getenv "HOME" with _ -> "") ".krobot-controller-history"

let save_history history =
  Lwt_read_line.save_history history_file_name history

let rec loop interpreter completion history =
  lwt key = read_key () in
  if key = key_escape then
    save_history history
  else
    match Command.of_key key with
      | Command.Accept_line ->
          let line = Text.strip (Engine.all_input (React.S.value engine_state)) in
          if line = "exit" then
            save_history history
          else if line <> "" then begin
            let history = Lwt_read_line.add_entry line history in
            set_engine_state (Engine.init history);
            lwt () = Lwt_log.notice line in
            ignore (Krobot_script.exec ~interpreter ~logger:(fun line -> log_add_line line; return ()) ~command:line);
            loop interpreter completion history
          end else
            loop interpreter completion history
      | Command.Complete ->
          let engine_state = Engine.reset (React.S.value engine_state) in
          let comp = React.S.value completion in
          set_engine_state { engine_state with Engine.mode = Engine.Edition comp.comp_state };
          loop interpreter completion history
      | command ->
          set_engine_state (Engine.update (React.S.value engine_state) command ());
          loop interpreter completion history

(* +-----------------------------------------------------------------+
   | Service monitoring                                              |
   +-----------------------------------------------------------------+ *)

let services, set_services = React.S.create []
let set_services l = set_services (List.sort Pervasives.compare l)

let check_services bus =
  lwt l = OBus_bus.list_names bus in
  set_services (List.fold_left (fun acc name ->
                                  if Text.starts_with name "fr.krobot." then
                                    String.sub name 10 (String.length name - 10) :: acc
                                  else
                                    acc) [] l);
  return ()

(* +-----------------------------------------------------------------+
   | State of all sensors/properties of the robot                    |
   +-----------------------------------------------------------------+ *)

type state = {
  box : Lwt_read_line.Terminal.box;
  services : string list;
  logs : Lwt_term.styled_text list;
  size : Lwt_term.size;
  engine_state : Lwt_read_line.Engine.state;
  color : [ `Yellow | `Blue ] option;
  infrareds : int array option;
  logic_sensors : bool array option;
  range_finders : int array option;
  card_interface : [ `Up | `Down ] option;
  card_sensors : [ `Up | `Down ] option;
  card_motors : [ `Up | `Down ] option;
  card_monitoring : [ `Up | `Down ] option;
  card_rx64 : [ `Up | `Down ] option;
  inhibited_forward : bool option;
  inhibited_backward : bool option;
}

(* +-----------------------------------------------------------------+
   | Drawing                                                         |
   +-----------------------------------------------------------------+ *)

let current_points = ref [||]

(* Draw the whole screen *)
let draw state =
  let size = state.size in
  let screen = Zone.make ~width:size.columns ~height:size.lines in
  let points = Zone.points screen in

  let line_color = lblue in
  let line = { blank with style = { blank.style with foreground = line_color } } in
  let name_color = lwhite in

  (* ===== Borders ===== *)

  for i = 1 to size.columns - 2 do
    points.(0).(i) <- { line with char = "─" };
    points.(size.lines - 1).(i) <- { line with char = "─" }
  done;
  for i = 1 to size.lines - 2 do
    points.(i).(0) <- { line with char = "│" };
    points.(i).(size.columns - 1) <- { line with char = "│" }
  done;
  points.(0).(0) <- { line with char = "┌" };
  points.(size.lines - 1).(0) <- { line with char = "└" };
  points.(size.lines - 1).(size.columns - 1) <- { line with char = "┘" };
  points.(0).(size.columns - 1) <- { line with char = "┐" };

  (* ===== Status ===== *)

  Draw.textc screen 1 0 [fg line_color; text "─[ ";
                         fg name_color; text "Range finders";
                         fg line_color; text " ]─┬─[ ";
                         fg name_color; text "Logic Sensors";
                         fg line_color; text " ]─┬─[ ";
                         fg name_color; text "Services";
                         fg line_color; text " ]─┬─[ ";
                         fg name_color; text "Cards";
                         fg line_color; text " ]─┬─[ ";
                         fg name_color; text "Status";
                         fg line_color; text  " ]─"];
  points.(9).(0) <- { line with char = "├" };
  points.(9).(size.columns - 1) <- { line with char = "┤" };
  for i = 1 to size.columns - 2 do
    points.(9).(i) <- { line with char = "─" }
  done;
  for i = 1 to 8 do
    points.(i).(20) <- { line with char = "│" };
    points.(i).(40) <- { line with char = "│" };
    points.(i).(55) <- { line with char = "│" };
    points.(i).(67) <- { line with char = "│" }
  done;
  Draw.textc screen 1 9 [fg line_color; text "───────────────────┴───────────────────┴──────────────┴───────────┴"];

  let zone = Zone.inner screen in

  begin
    match state.range_finders with
      | Some range_finders ->
          for i = 0 to 7 do
            Draw.textc zone 0 i  [textf "%d : %d" i range_finders.(i)]
          done
      | None ->
          for i = 0 to 7 do
            Draw.textc zone 0 i [fg red; text "unavailable"]
          done
  end;

  begin
    match state.logic_sensors with
      | Some logic_sensors ->
          for i = 0 to 7 do
            let j = i * 2 in
            Draw.textf zone 20 i  "%02d : %s  %02d : %s"
              (j + 0) (if logic_sensors.(j + 0) then "O" else ".")
              (j + 1) (if logic_sensors.(j + 1) then "O" else ".")
          done
      | None ->
          for i = 0 to 7 do
            Draw.textc zone 20 i [fg red; text "unavailable"]
          done
  end;

  let zone' = Zone.sub ~zone ~x:40 ~y:0 ~width:14 ~height:8 in
  let rec loop y = function
    | [] ->
        ()
    | name :: rest ->
        Draw.text ~zone:zone' ~x:0 ~y ~text:name;
        loop (y + 1) rest
  in
  loop 0 state.services;

  let x = 55 in
  let text_of_state name = function
    | Some `Down -> [fg lred; text name]
    | Some `Up -> [text name]
    | None -> [fg red; text "unavailable"]
  in
  Draw.textc zone x 0 (text_of_state "interface" state.card_interface);
  Draw.textc zone x 1 (text_of_state "sensor" state.card_sensors);
  Draw.textc zone x 2 (text_of_state "motor" state.card_motors);
  Draw.textc zone x 3 (text_of_state "monitoring" state.card_monitoring);
  Draw.textc zone x 4 (text_of_state "rx64" state.card_rx64);

  let x = x + 12 in
  begin
    match state.color with
      | Some color ->
          Draw.textc zone x 0 [text "color: "; text (match color with
                                                       | `Yellow -> "yellow"
                                                       | `Blue -> "blue")]
      | None ->
          Draw.textc zone x 0 [fg red; text "unavailable"]
  end;
  let text_of_motor_state mode = function
    | Some true ->
        [text mode; fg lyellow; text "inhibited"]
    | Some false ->
        [text mode; text "OK"]
    | None ->
        [fg red; text "unavailable"]
  in
  Draw.textc zone x 1 (text_of_motor_state "move forward: " state.inhibited_forward);
  Draw.textc zone x 2 (text_of_motor_state "move backward: " state.inhibited_backward);

  begin
    match state.infrareds with
      | Some infrareds ->
          for i = 0 to 3 do
            Draw.textc zone x (i + 3) [textf "infrared %d: %d" i infrareds.(i)]
          done
      | None ->
          for i = 3 to 6 do
            Draw.textc zone x i [fg red; text "unavailable"]
          done
  end;

  (* ===== History ===== *)

  let zone = Zone.sub ~zone:screen ~x:1 ~y:10 ~width:(Zone.width screen - 2) ~height:(Zone.height screen - 15) in
  let rec loop y = function
    | [] ->
        ()
    | line :: rest ->
        if y < 0 then
          ()
        else begin
          Draw.textc zone 0 y line;
          loop (y - 1) rest
        end
  in
  loop (Zone.height zone - 1) state.logs;

  (* ===== Read-line ===== *)

  points.(size.lines - 3).(0) <- { line with char = "├" };
  points.(size.lines - 3).(size.columns - 1) <- { line with char = "┤" };
  points.(size.lines - 5).(0) <- { line with char = "├" };
  points.(size.lines - 5).(size.columns - 1) <- { line with char = "┤" };
  for i = 1 to size.columns - 2 do
    points.(size.lines - 5).(i) <- { line with char = "─" };
    points.(size.lines - 3).(i) <- { line with char = "─" }
  done;

  let zone = Zone.sub ~zone:screen ~x:1 ~y:(size.lines - 4) ~width:(size.columns - 2) ~height:1 in
  let engine_state = state.engine_state in
  let cursor_position =
    match engine_state.Engine.mode with
      | Engine.Edition(before, after) ->
          let len = Text.length before in
          Draw.textc zone 0 0 [Text before; Text after];
          len
      | Engine.Selection state ->
          let a = min state.Engine.sel_cursor state.Engine.sel_mark
          and b = max state.Engine.sel_cursor state.Engine.sel_mark in
          let part_before = Text.chunk (Text.pointer_l state.Engine.sel_text) a
          and part_selected = Text.chunk a b
          and part_after = Text.chunk (Text.pointer_r state.Engine.sel_text) b in
          Draw.textc zone 0 0 [Text part_before; Underlined; Text part_selected; Reset; Text part_after];
          if state.Engine.sel_cursor < state.Engine.sel_mark then
            Text.length part_before
          else
            Text.length part_before + Text.length part_selected
      | Engine.Search state ->
          let len = Text.length state.Engine.search_word in
          Draw.text zone 0 0 (Printf.sprintf "(reverse-i-search)'%s'" state.Engine.search_word);
          begin match state.Engine.search_history with
            | [] ->
                19 + len
            | phrase :: _ ->
                let ptr_start = match Text.find phrase state.Engine.search_word with
                  | Some ptr ->
                      ptr
                  | None ->
                      assert false
                in
                let ptr_end = Text.move len ptr_start in
                let before = Text.chunk (Text.pointer_l phrase) ptr_start
                and selected = Text.chunk ptr_start ptr_end
                and after =  Text.chunk ptr_end (Text.pointer_r phrase) in
                Draw.textc zone (20 + len) 0 [
                  Text ": ";
                  Text before;
                  Underlined;
                  Text selected;
                  Reset;
                  Text after;
                ];
                19 + len
          end
  in
  Draw.map zone cursor_position 0 (fun point -> { point with style = { point.style with inverse = true } });

  let zone = Zone.sub ~zone:screen ~x:1 ~y:(size.lines - 3) ~width:(size.columns - 2) ~height:3 in
  begin
    match state.box with
      | Terminal.Box_none | Terminal.Box_empty ->
          ()
      | Terminal.Box_message msg ->
          Draw.text zone 0 1 msg
      | Terminal.Box_words(words, _) ->
          ignore (TextSet.fold
                    (fun word i ->
                       let len = Text.length word in
                       Draw.text zone i 1 word;
                       let i = i + len in
                       Draw.textc zone i 0 [fg line_color; text "┬"];
                       Draw.textc zone i 1 [fg line_color; text "│"];
                       Draw.textc zone i 2 [fg line_color; text "┴"];
                       i + 1)
                    words 0)
  end;

  lwt () = Lwt_term.render_update !current_points (Zone.points screen) in
  current_points := Zone.points screen;
  return ()

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

lwt () =
  Krobot_arg.parse ();

  lwt () = Lwt_log.notice "connecting to the krobot bus..." in
  lwt krobot = Krobot.create () and bus = Krobot_dbus.open_bus () in

  lwt interpreter = Krobot_script.make krobot in
  let engine_mode = React.S.map (fun state -> state.Engine.mode) engine_state in
  let completion =
    Krobot_script.completion
      interpreter
      (React.S.fmap
         (function
            | Engine.Edition state -> Some state
            | _ -> None)
         ("", "")
         engine_mode)
  in
  let box =
    React.S.l2
      (fun mode comp ->
         match mode with
           | Engine.Edition _ ->
               Terminal.Box_words(comp.comp_words, 0)
           | Engine.Selection _ ->
               Terminal.Box_message "<selection>"
           | Engine.Search _ ->
               Terminal.Box_message "<backward search>")
      engine_mode completion
  in

  (* Put the terminal into drawing mode: *)
  lwt () = Lwt_term.enter_drawing_mode () in
  lwt () = Lwt_term.hide_cursor () in

  init_logger ();
  redirect_stderr ();

  (* Dump all logs to stdout on abnormal exit: *)
  let node =
    Lwt_sequence.add_l
      (fun () ->
         lwt () = Lwt_term.leave_drawing_mode () in
         Lwt_list.iter_s printlc (List.rev (React.S.value logs)))
      Lwt_main.exit_hooks
  in

  (* Service monitoring *)
  lwt () = Lwt_event.always_notify_p (fun _ -> check_services bus) =|< OBus_signal.connect (OBus_bus.name_owner_changed bus) in

  (* State of robot *)
  let monitor name dummy make =
    React.S.switch
      (React.S.const dummy)
      (Lwt_event.map_s
         (function
            | "" ->
                return (React.S.const dummy)
            | _ ->
                try_lwt
                  make ()
                with exn ->
                  lwt () = Lwt_log.error_f ~exn "signal maker failed with" in
                  raise_lwt exn)
         (Lwt_signal.delay (OBus_resolver.make bus name)))
  in
  let state =
    React.S.l4
      (fun
         (box, services, logs, engine_state, size)
         (card_interface, card_sensors, card_motors, card_monitoring, card_rx64)
         (color, infrareds, logic_sensors, range_finders)
         (inhibited_forward, inhibited_backward) ->
           {
             box = box;
             logs = logs;
             size = size;
             engine_state = engine_state;
             services = services;
             color = color;
             infrareds = infrareds;
             logic_sensors = logic_sensors;
             range_finders = range_finders;
             card_interface = card_interface;
             card_sensors = card_sensors;
             card_motors = card_motors;
             card_monitoring = card_monitoring;
             card_rx64 = card_rx64;
             inhibited_forward = inhibited_forward;
             inhibited_backward = inhibited_backward;
           })
      (React.S.l5 (fun a b c d e -> (a, b, c, d, e)) box services logs engine_state size)
      (monitor "fr.krobot.Driver" (None, None, None, None, None)
         (fun () ->
            let make card = OBus_property.monitor (Krobot_driver.Card.state (card krobot)) in
            lwt interface = make Krobot_driver.card_interface
            and sensors = make Krobot_driver.card_sensors
            and motors = make Krobot_driver.card_motors
            and monitoring = make Krobot_driver.card_monitoring
            and rx64 = make Krobot_driver.card_rx64 in
            return (React.S.l5
                      (fun a b c d e -> (Some a, Some b, Some c, Some d, Some e))
                      interface sensors motors monitoring rx64)))
      (monitor "fr.krobot.Service.Sensors" (None, None, None, None)
         (fun () ->
            lwt color = OBus_property.monitor (Krobot_sensors.color krobot)
            and infrareds = OBus_property.monitor (Krobot_sensors.infrareds krobot)
            and logic_sensors = OBus_property.monitor (Krobot_sensors.logic_sensors krobot)
            and range_finders = OBus_property.monitor (Krobot_sensors.range_finders krobot) in
            return (React.S.l4 (fun a b c d -> (Some a, Some b, Some c, Some d)) color infrareds logic_sensors range_finders)))
      (monitor "fr.krobot.Service.Motors" (None, None)
         (fun () ->
            lwt inhibited_forward = OBus_property.monitor (Krobot_motors.inhibited_forward krobot)
            and inhibited_backward = OBus_property.monitor (Krobot_motors.inhibited_backward krobot) in
            return (React.S.l2 (fun a b -> (Some a, Some b)) inhibited_forward inhibited_backward)))
  in

  (* Initialize the list of running services: *)
  ignore (check_services bus);

  (* Redraw the screen when the state changes *)
  Lwt_signal.always_notify_s draw state;

  (* Display log comming from daemons *)
  let daemons = OBus_proxy.make (OBus_peer.anonymous bus) ["fr"; "krobot"; "Daemon"] in
  lwt () =
    Lwt_event.always_notify
      (fun msg -> log_add_lines (List.map (fun line -> [text line]) (Text.split ~sep:"\n" msg)))
    =|< OBus_signal.connect (OBus_signal.make Krobot_dbus_daemon.Fr_krobot_Daemon.s_log daemons)
  in

  lwt history = Lwt_read_line.load_history history_file_name in
  set_engine_state (Engine.init history);

  (* User input loop *)
  lwt () = Lwt_term.with_raw_mode (fun () -> loop interpreter completion history) in

  (* Normal exit, do not dump logs on stdout: *)
  Lwt_sequence.remove node;

  (* Leave drawing mode *)
  Lwt_term.leave_drawing_mode ()
