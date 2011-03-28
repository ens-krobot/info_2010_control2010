(*
 * krobot_arg.mli
 * --------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(** Command line arguments parsing *)

(** This module offers a convenient way of dealing with command line
    argument. It also defines options that are common to all krobot
    programs. *)

(** {6 Argument registration} *)

type section = string
    (** Type of a section. Section are used to hierarchically organize
        options. *)

(** The following functions register an argument an returns a lazy
    value that must be forced after the call to {!parse}. Functions
    sufixed by [_d] take a default value. Other functions rerturns an
    option. *)

type 'a opt = ?section : section -> key : string -> ?doc : string -> unit -> 'a option Lazy.t
  (** Type of a function which register an option returning without a
      default value.

      - [section] is the section of the option. If not provided it
        defaults to the program name.
      - [key] is the name of the option, such as [-distance], or [-d]
      - [doc] is a short description of the option, which is printed
        when the user invokes the program with [-help] or [--help]
  *)

type 'a opt_d = ?section : section -> key : string -> ?doc : string -> default : 'a -> unit -> 'a Lazy.t
  (** Type of a function which register an option returning with a
      default value. *)

val flag : bool opt
val flag_d : bool opt_d

val nflag : bool opt
val nflag_d : bool opt_d

val int : int opt
val int_d : int opt_d

val float : float opt
val float_d : float opt_d

val string : string opt
val string_d : string opt_d

val keyword : keywords : (string * 'a) list -> 'a opt
val keyword_d : keywords : (string * 'a) list -> 'a opt_d

(** {6 Parsing} *)

val parse : unit -> unit
  (** Parses command line arguments. *)

(** {6 Help} *)

val usage : string -> 'a
  (** [usage message] prints an error message and exit *)

val help : unit -> 'a
  (** [help ()] prints the help of the program and exit *)
