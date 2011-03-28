(*
 * krobot_boardname.mli
 * --------------------
 * Copyright : (c) 2009, Stéphane Glondu <steph@glondu.net>
 * Licence   : BSD3
 *
 * This file is a part of Krobot.
 *)

val get_board_name : string -> string option
  (** [get_board_name dump] recherche dans le [dump] mémoire (peut
      être également un fichier .hex chargé) le nom de la carte. *)
