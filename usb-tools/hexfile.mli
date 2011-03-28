(*
 * krobot_hexfile.mli
 * ------------------
 * Copyright : (c) 2009, Stéphane Glondu <steph@glondu.net>
 * Licence   : BSD3
 *
 * This file is a part of Krobot.
 *)

type hex_record =
  | Data of int * string
  | ExtendedLinearAddress of int
  | EndOfFile of int

val parse_file : string -> hex_record list Lwt.t

val print_record : hex_record -> unit
  (** Prints one record on standard output. *)

val validate_and_copy : hex_record list -> int -> string -> int -> int -> unit
  (** [validate_and_copy hex addr_base buffer offset length] copies
      the contents of the (parsed) [hex] file to [buffer]. [offset]
      and [length] denote the valid range inside [buffer] that can be
      written. [addr_base] is the address [buffer] is mapped to on the
      microcontroller. Bytes outside the range are ignored (and a
      warning is printed on standard error. *)
