(*
 * krobot.ml
 * ---------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

type t = OBus_bus.t

external to_bus : t -> OBus_bus.t = "%identity"
external of_bus : OBus_bus.t -> t = "%identity"

let krobot = lazy(Krobot_dbus.open_bus ())
let create () = Lazy.force krobot
