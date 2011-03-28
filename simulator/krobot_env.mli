(*
 * krobot_env.mli
 * --------------
 * Copyright : (c) 2010, Xavier Lagorce <Xavier.Lagorce@crans.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

type t

type state = {
  x : float;
  y : float;
  theta : float;
}

val create : state -> t

val move : t -> float -> float -> float -> unit

val turn : t -> float -> float -> float -> unit

val set_velocities : t -> float -> float -> float -> unit

val get_velocities : t-> float * float

val get_encoders : t -> float * float

val get_state : t -> state

val draw : t -> Cairo.t -> float -> float -> unit

