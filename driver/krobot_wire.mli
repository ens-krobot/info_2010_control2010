(*
 * krobot_wire.mli
 * ---------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

(** Serialization/deserialization stuff *)

(** {6 Convertors} *)

type 'a convertor
  (** Type of a convertor. A convertor is used to serialize and
      deserialize caml values of type ['a] *)

val int8 : int convertor
val uint8 : int convertor
val int16 : int convertor
val uint16 : int convertor
val int32 : int convertor
val uint32 : int convertor

val boolean : bool convertor
val array : int -> 'a convertor -> 'a array convertor
val string : string convertor

val unit : unit convertor

val seq2 : 'a1 convertor -> 'a2 convertor -> ('a1 * 'a2) convertor
val seq3 : 'a1 convertor -> 'a2 convertor -> 'a3 convertor -> ('a1 * 'a2 * 'a3) convertor
val seq4 : 'a1 convertor -> 'a2 convertor -> 'a3 convertor -> 'a4 convertor -> ('a1 * 'a2 * 'a3 * 'a4) convertor
val seq5 : 'a1 convertor -> 'a2 convertor -> 'a3 convertor -> 'a4 convertor -> 'a5 convertor -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5) convertor
val seq6 : 'a1 convertor -> 'a2 convertor -> 'a3 convertor -> 'a4 convertor -> 'a5 convertor -> 'a6 convertor -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) convertor
val seq7 : 'a1 convertor -> 'a2 convertor -> 'a3 convertor -> 'a4 convertor -> 'a5 convertor -> 'a6 convertor -> 'a7 convertor -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7) convertor
val seq8 : 'a1 convertor -> 'a2 convertor -> 'a3 convertor -> 'a4 convertor -> 'a5 convertor -> 'a6 convertor -> 'a7 convertor -> 'a8 convertor -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8) convertor

(** {6 Reading/writing values} *)

val write : 'a convertor -> 'a -> string
  (** [write conv v] returns a string containing the value [v]
      serialized *)

val read : 'a convertor -> string -> 'a
  (** [read conv buffer] deserializes the value contained in [buffer]
      using [conv] *)
