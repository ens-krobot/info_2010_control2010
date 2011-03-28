(*
 * krobot_script.mli
 * -----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(** Minit script language for the monitor *)

type interpreter
  (** Type of a script interpreter *)

val make : Krobot.t -> interpreter Lwt.t
  (** Creates an interpreter from the given krobot object *)

val completion : interpreter -> Lwt_read_line.edition_state React.signal -> Lwt_read_line.completion_result React.signal
  (** [completion interpreter edition_state] takes a signal holding
      the current edition state and returns a signal holding the
      current completion. The completion is updated dynamically when
      services come up/down. *)

val exec : interpreter : interpreter -> logger : (Lwt_term.styled_text -> unit Lwt.t) -> command : string -> unit Lwt.t
  (** [exec ~interpreter ~logger ~command] parses [command] and
      execute it. The result is logged with [logger]. *)
