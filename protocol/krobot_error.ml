(*
 * error.ml
 * --------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

module Device =
struct
  exception Error of string
    with obus("fr.krobot.Device.Error.Error")

  exception Closed of string
    with obus("fr.krobot.Device.Error.Closed")

  exception Unavailable of string
    with obus("fr.krobot.Device.Error.Unavailable")

  exception Not_implemented of string
    with obus("fr.krobot.Device.Error.NotImplemented")

  exception Timeout of string
    with obus("fr.krobot.Device.Error.Timeout")
end
