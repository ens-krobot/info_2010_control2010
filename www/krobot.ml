open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml

open Kard

let card_type = function
  | "USB Development Board" -> `test
  | "Proximity Sensor" -> `sensor
  | "Motor Controller" -> `motor
  | "Robot Interface" -> `interface

(* Services declaration *)
let serv_home = Eliom_services.new_service ~path:[""]
  ~get_params:Eliom_parameters.unit ()

let serv_home_upload = Eliom_services.new_service ~path:["update"]
  ~get_params:(user_type card_type card_name "card") ()

let serv_home_update = Eliom_services.new_post_service
  ~fallback:serv_home_upload
  ~post_params:(file "firmware") ()

let serv_config = Eliom_services.new_service ~path:["config"]
  ~get_params:Eliom_parameters.unit ()

let serv_config_post = Eliom_services.new_post_service
  ~fallback:serv_config
  ~post_params:(int "left_kp" ** int "left_ki" ** int "left_kd" ** int "left_il" **
                  int "right_kp" ** int "right_ki" ** int "right_kd" ** int "right_il") ()

let serv_sensors = Eliom_services.new_service ~path:["sensors"]
  ~get_params:Eliom_parameters.unit ()

let serv_sensors_calibrate = Eliom_services.new_service ~path:["sensors_calibrate"]
  ~get_params:(int "rangefinder") ()

let serv_sensors_view_cal = Eliom_services.new_service ~path:["sensors_view_cal"]
  ~get_params:(int "rangefinder") ()

let serv_control = Eliom_services.new_service ~path:["control"]
  ~get_params:Eliom_parameters.unit ()

(* Common page elements *)
let div_with_id id ?(a = []) l = div ~a:(a_id id :: a) l

let header sp = (
  head
    (title (pcdata "Mobile Robot Platform"))
    [css_link ~uri:(make_uri ~service:(static_dir sp) ~sp ["stylesdocs.css"]) ();
     js_script ~uri:(make_uri ~service:(static_dir sp) ~sp ["ajax.js"]) ()]
)

let bottom sp = (
  p [br ();
     pcdata "Visit the project home page: ";
     XHTML.M.a ~a:[a_class ["external"]; a_href (uri_of_string "http://sourceforge.net/projects/krobot/")] [pcdata "http://sourceforge.net/projects/krobot/"];
     pcdata "."
    ]
)

let side_menu cur_menu sp =
  Eliom_tools.menu ~classe:["mainmenu"]
    (serv_home, [pcdata "Home"]) [
      (serv_config, [pcdata "Krobot_config"]);
      (serv_sensors, [pcdata "Sensors"]);
      (serv_control, [pcdata "Control"])
    ] ~service:cur_menu ~sp

let sidebar cur_menu sp = div_with_id "sidebar" [
  img ~alt:"" ~src:(make_uri ~service:(static_dir sp) ~sp ["krobot.png"]) ();
  (side_menu cur_menu sp)
]

(* Service home *)
let motor_controller_version =
  Lwt_unix.run (get_firmware_build kMotor)
let proximity_sensor_version =
  Lwt_unix.run (get_firmware_build kSensor)
let robot_interface_version =
  Lwt_unix.run (get_firmware_build kInterface)

let kernel_version = Lwt_unix.run (Lwt_process.pread (Lwt_process.shell "uname -r"))

let _ = Eliom_predefmod.Xhtml.register serv_home
  (fun sp () () ->
     Lwt.return
       (html
          (header sp)
          (body [
             div_with_id "page" [
               div_with_id "content" [
                 h1 [pcdata "Mobile Robot Platform"];
                 p [pcdata ("Motor Controller firmware build [");
                    a ~service:serv_home_upload ~sp [pcdata "Update"] (`motor); pcdata "]: ";
                    code [pcdata motor_controller_version];
                    br ();
                    pcdata ("Proximity Sensor firmware build [");
                    a ~service:serv_home_upload ~sp [pcdata "Update"] (`sensor); pcdata "]: ";
                    code [pcdata proximity_sensor_version];
                    br ();
                    pcdata ("Robot Interface firmware build [");
                    a ~service:serv_home_upload ~sp [pcdata "Update"] (`interface); pcdata "]: ";
                    code [pcdata robot_interface_version];
                   ];
                 p [pcdata ("Kernel version: "); code [pcdata kernel_version]];
                 (bottom sp)
               ];
               (sidebar serv_home sp)
             ]
           ])
       )
  )

let create_upload_form sp card (firmware) =
  let file_label = "firmware" in
  [p [label ~a:[a_for file_label] [
        pcdata "Firmware file (.hex): ";
        file_input ~name:firmware ~a:[a_id file_label] ();
      ]];
   p [a ~service:serv_home ~sp [input ~a:[a_input_type `Button; a_value "Cancel"] ()] ();
      pcdata " ";
      input ~a:[a_input_type `Submit; a_value "Update now!"] ()
     ]]

let update_handler card sp posted =
  let form = Eliom_predefmod.Xhtml.post_form ~service:serv_home_update ~sp (create_upload_form sp card) (card) in
  let res =
    match posted with
      | Some (firmware, res) ->
          p ~a:[a_class ["notice"]] [pcdata "New firmware uploaded. You have to flash the card manually."]
      | None -> p []
  in
  Lwt.return
    (html
       (header sp)
       (body [
          div_with_id "page" [
            div_with_id "content" [
              h1 [pcdata "Mobile Robot Platform"];
              h2 [pcdata "Firmware update for "; em [pcdata (card_name card)]];
              (res);
              (form);
              (bottom sp)
            ];
            (sidebar serv_home sp)
          ]
        ])
      )

let _ = Eliom_predefmod.Xhtml.register serv_home_upload
  (fun sp card () -> update_handler card sp None)

let _ = Eliom_predefmod.Xhtml.register serv_home_update
  (fun sp card firmware ->
     let newname = ("/tmp/" ^ (card_name card) ^ ".hex") in
     (try
        Unix.unlink newname;
      with _ -> ());
     Unix.link (Eliom_sessions.get_tmp_filename firmware) newname;
     if (card = `sensor) then
       Lwt_unix.run (bootloader kSensor)
     else if (card = `motor) then
       Lwt_unix.run (bootloader kMotor)
     else if (card = `interface) then
       Lwt_unix.run (bootloader kInterface);
     let res = Lwt_unix.run (Lwt_process.pread (Lwt_process.shell ("/home/olivier/krobot/PC_Mainboard/Apps/usb/send_firmware.native " ^ newname))) in
     let k = Lwt_unix.run (Bootloader.open_card ()) in
     Lwt_unix.run (Bootloader.reset_board k);
     update_handler card sp (Some (firmware, res))
  )

(** Equivalent of String.concat for lists *)
let rec list_concat sep = function
  | [] -> []
  | [_] as x -> x
  | x::xs -> x::sep::(list_concat sep xs)

(* Service config *)
let create_form (right, left) (left_kp, (left_ki, (left_kd, (left_il, (right_kp, (right_ki, (right_kd, right_il))))))) =
  let motorset my_id my_legend items =
    fieldset ~a:[a_id my_id]
      (List.flatten [
         [legend [pcdata my_legend]];
         (list_concat (br ())
            (List.map
               (fun (my_label, my_title, name, value) ->
                  label ~a:[a_for my_label] [
                    pcdata my_title;
                    int_input ~input_type:`Text ~name ~value ~a:[a_id my_label] ()
                  ])
               items))
       ])
  in [
    motorset "paramMotorLeft" "Left motor" [
      "left_kp_label", "KP: ", left_kp, left.kp;
      "left_ki_label", "KI: ", left_ki, left.ki;
      "left_kd_label", "KD: ", left_kd, left.kd;
      "left_il_label", "IL: ", left_il, left.il;
    ];
    motorset "paramMotorRight" "Right motor" [
      "right_kp_label", "KP: ", right_kp, right.kp;
      "right_ki_label", "KI: ", right_ki, right.ki;
      "right_kd_label", "KD: ", right_kd, right.kd;
      "right_il_label", "IL: ", right_il, right.il;
    ];
    p  ~a:[a_id "paramMotorSubmit"] [input ~a:[a_input_type `Submit; a_value "Change"] ()];
  ]

let config_handler sp posted =
  traj_read_config kMotor >>= fun motors ->
    let form = Eliom_predefmod.Xhtml.post_form ~service:serv_config_post ~sp (create_form motors) () in
    let res =
      match posted with
        | Some (left_kp, (left_ki, (left_kd, (left_il, (right_kp, (right_ki, (right_kd, right_il))))))) ->
            (* left_kp, etc. sont accessibles ici *)
            p ~a:[a_class ["notice"]] [pcdata "Motor controller parameters updated! The LM629 have to be re-initialized for the changes to take effect."]
        | None -> p []
    in
    Lwt.return
      (html
         (header sp)
         (body [
            div_with_id "page" [
              div_with_id "content" [
                h1 [pcdata "Mobile Robot Platform"];
                h2 [pcdata "Krobot_config"];
                (res);
                (form);
                (bottom sp)
              ];
              (sidebar serv_config sp)
            ]
          ])
      )

let _ = Eliom_predefmod.Xhtml.register serv_config
  (fun sp () () -> config_handler sp None)

let _ = Eliom_predefmod.Xhtml.register serv_config_post
  (fun sp () ((left_kp, (left_ki, (left_kd, (left_il, (right_kp, (right_ki, (right_kd, right_il))))))) as x) ->
     Lwt_unix.run (traj_config kMotor `left {kp = left_kp; ki = left_ki; kd = left_kd; il = left_il});
     Lwt_unix.run (traj_config kMotor `right {kp = right_kp; ki = right_ki; kd = right_kd; il = right_il});
     config_handler sp (Some x)
  )

(* Service sensors *)
let system_sensors () = Lwt_process.pread (Lwt_process.shell "sensors")
let get_compass_value () =
  let compass = (get_compass kInterface) in
  compass >>= fun s -> return (string_of_float ((float_of_int s) /. 10.0))

let sensors_handler sp posted =
  get_compass_value () >>= fun compass_value ->
  get_rangefinder_state kSensor >>= fun rangefinder_value ->
  get_tor_state kSensor >>= fun tor_value ->
  system_sensors () >>= fun system_sensors ->
    let ajax_update = (
      script ~contenttype:"text/javascript"
        (cdata_script "function update() {
  ajaxMakeRequest('./sensors_update');
  setTimeout('update()', 200);
}
update();"
        )) in
    let rangefinders items =
      p ~a:[a_id "sensors_rangefinder"]
        (List.flatten
           (list_concat [br ()]
              (List.map
                 (fun id -> [
                    pcdata (" Rangefinder " ^ (string_of_int (id+1)) ^ " [");
                    unsafe_data ("<a href=\"" ^ (string_of_uri (make_uri ~service:serv_sensors_calibrate ~sp (id))) ^ "\" onclick=\"return confirm('Please make sure the rangefinder " ^ (string_of_int (id+1)) ^ " is far from any obstacle.');\">Cal.</a>");
                    (* a ~service:serv_sensors_calibrate ~sp [pcdata "Cal."] (id); *)
                    pcdata ", ";
                    a ~service:serv_sensors_view_cal ~sp [pcdata "View cal."] (id);
                    pcdata "]: ";
                    img ~alt:"" ~a:[a_id ("sensors_rangefinder_bar_" ^ (string_of_int id)); a_height (`Pixels 16); a_width (`Pixels (rangefinder_value.(id) / 10))] ~src:(make_uri ~service:(static_dir sp) ~sp ["bargraph.png"]) ();
                    code ~a:[a_id ("sensors_rangefinder_" ^ (string_of_int id))] [pcdata (" " ^ (string_of_float (float_of_int rangefinder_value.(id) /. 10.)) ^ " cm")];
                  ])
                 items)
           )) in
    let tors items =
      p ~a:[a_id "sensors_tor"]
        (List.flatten [
           [img ~alt:"" ~src:(make_uri ~service:(static_dir sp) ~sp ["robot.png"]) ()];
           (List.map
              (fun id ->
                 let arg = match tor_value.(id) with
                     false -> "tor_off"
                   | true -> "tor_on" in
                 code ~a:[a_id ("sensors_tor_" ^ (string_of_int id)); a_class [arg]] [pcdata ("TOR " ^ (string_of_int id))]
              )
              items)
         ]) in
    let res =
      match posted with
        | Some (rangefinder, data, status) ->
            let res = [
              pcdata (Printf.sprintf "Start threshold: %d (= %01.3f * Vref)" (get_byte data 0) ((float_of_int (get_byte data 0)) /. 24.)); br();
              pcdata (Printf.sprintf "Minimal threshold: %d (= %01.3f * Vref)" (get_byte data 1) ((float_of_int (get_byte data 1)) /. 24.)); br();
              pcdata (Printf.sprintf "Delay before reception: %d us" (get_int16 data 2)); br();
              pcdata (Printf.sprintf "Slope [~ sound speed]: %d m/s" (get_int16 data 4)); br();
              pcdata (Printf.sprintf "Offset: %d (mm)" (get_int16 data 6))] in
            if (status = Protocol.cal_error) then
              p ~a:[a_class ["warning"]] (List.flatten [
                [pcdata ("Calibration error for rangefinder " ^ (string_of_int (rangefinder+1)) ^ ". Unable to calibrate!"); br()];
                res
              ])
            else if (status = Protocol.cal_done) then
              p ~a:[a_class ["notice"]] (List.flatten [
                [pcdata ("Calibration done for rangefinder " ^ (string_of_int (rangefinder+1)) ^ "!"); br()];
                res
              ])
            else
              p ~a:[a_class ["notice"]] (List.flatten [
                [pcdata ("Calibration data for rangefinder " ^ (string_of_int (rangefinder+1)) ^ "."); br()];
                res
              ])
        | None -> p []
    in
    Lwt.return
      (html
         (header sp)
         (body [
            (ajax_update);
            div_with_id "page" [
              div_with_id "content" [
                h1 [pcdata "Mobile Robot Platform"];
                h2 [pcdata "Sensors"];
                p [pcdata "Compass: "; code ~a:[a_id "sensors_compass"] [pcdata (compass_value ^ "°")]];
                (res);
                (rangefinders [0; 1; 2; 3; 4; 5; 6; 7]);
                p [pcdata "TOR:"];
                (tors [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15]);
                hr ();
                p [pcdata "Mainboard:"];
                pre [pcdata system_sensors];
                (bottom sp)
              ];
              (sidebar serv_sensors sp)
            ]
          ])
      )

let _ = Eliom_predefmod.Xhtml.register serv_sensors
  (fun sp () () -> sensors_handler sp None)

let _ = Eliom_predefmod.Xhtml.register serv_sensors_calibrate
  (fun sp rangefinder () ->
     cal_rangefinder_start kSensor rangefinder >>= fun _ ->
     cal_rangefinder_continue kSensor >>= fun res ->
       let sub_res = String.sub res 1 8 in
       sensors_handler sp (Some (rangefinder, sub_res, get_byte res 0))
  )

let _ = Eliom_predefmod.Xhtml.register serv_sensors_view_cal
  (fun sp rangefinder () ->
     get_rangefinder_cal kSensor rangefinder >>= fun res ->
       sensors_handler sp (Some (rangefinder, res, -1))
  )

let _ = Eliom_predefmod.Text.register_new_service
  ~path:["sensors_update"]
  ~get_params:(int "ajax")
  (fun sp ajax () ->
     get_compass_value () >>= fun value ->
     get_rangefinder_state kSensor >>= fun rangefinder_value ->
     get_tor_state kSensor >>= fun tor_value ->
       let update_rangefinder id =
         "<elt id=\"sensors_rangefinder_" ^ (string_of_int id) ^ "\"> " ^ (string_of_float (float_of_int rangefinder_value.(id) /. 10.)) ^ " cm</elt>
<attr id=\"sensors_rangefinder_bar_" ^ (string_of_int id) ^ "\" name=\"width\">" ^ (string_of_int (rangefinder_value.(id) / 10)) ^ "px</attr>" in
       let update_tor id =
         let arg = match tor_value.(id) with
             false -> "tor_off"
           | true -> "tor_on" in
         "<attr id=\"sensors_tor_" ^ (string_of_int id) ^ "\" name=\"class\">" ^ arg ^ "</attr>" in
       Lwt.return (("<?xml version=\"1.0\" encoding=\"utf-8\"?>
<root>
<elt id=\"sensors_compass\">" ^ value ^ "°</elt>"
          ^ (update_rangefinder 0) ^ (update_rangefinder 1) ^ (update_rangefinder 2) ^ (update_rangefinder 3)
          ^ (update_rangefinder 4) ^ (update_rangefinder 5) ^ (update_rangefinder 6) ^ (update_rangefinder 7)
          ^ (update_tor 0) ^ (update_tor 1) ^ (update_tor 2) ^ (update_tor 3) ^ (update_tor 4) ^ (update_tor 5) ^ (update_tor 6) ^ (update_tor 7)
          ^ (update_tor 8) ^ (update_tor 9) ^ (update_tor 10) ^ (update_tor 11) ^ (update_tor 12) ^ (update_tor 13) ^ (update_tor 14) ^ (update_tor 15)
          ^ "</root>"), "text/xml")
  )

(* Service control *)
let _ = Eliom_predefmod.Xhtml.register serv_control
  (fun sp () () ->
     Lwt.return
       (html
          (header sp)
          (body [
             div_with_id "page" [
               div_with_id "content" [
                 h1 [pcdata "Mobile Robot Platform"];
                 h2 [pcdata "Control"];
                 (bottom sp)
               ];
               (sidebar serv_control sp)
             ]
           ])
       )
  )
