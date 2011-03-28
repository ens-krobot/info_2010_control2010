(*
 * krobot_device.ml
 * ----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

module type S = sig
  type t
  val name : t -> string
  val state : t -> [ `Up | `Down ] React.signal
  val close : t -> unit Lwt.t
end
