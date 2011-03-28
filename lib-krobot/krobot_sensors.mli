(*
 * krobot_sensors.mli
 * ------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

type color =
    [ `Blue
    | `Yellow ]
val color : Krobot.t -> (color, [ `readable ]) OBus_property.t
val infrareds : Krobot.t -> (int array, [ `readable ]) OBus_property.t
val logic_sensors : Krobot.t -> (bool array, [ `readable ]) OBus_property.t
val range_finders : Krobot.t -> (int array, [ `readable ]) OBus_property.t
