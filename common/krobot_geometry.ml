(*
 * krobot_geometry.ml
 * ------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

let pi = 4.0 *. atan 1.0

let principal angle =
  let angle = angle -. (floor (angle /. (2.0 *. pi)) *. 2.0 *. pi) in
  if angle > pi then
    angle -. 2.0 *. pi
  else
    angle
