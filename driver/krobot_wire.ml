(*
 * krobot_wire.ml
 * --------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of [kro]bot.
 *)

type pointer = {
  mutable offset : int;
  buffer : string;
}

type 'a convertor = {
  write : Buffer.t -> 'a -> unit;
  read : pointer -> 'a;
}

let get_uint8 pointer =
  let offset = pointer.offset in
  pointer.offset <- offset + 1;
  Char.code pointer.buffer.[offset]

let put_uint8 buffer value =
  Buffer.add_char buffer (Char.unsafe_chr value)

let int8 = {
  read = get_uint8;
  write = put_uint8;
}

let uint8 = int8

let int16 = {
  read = (
    fun pointer ->
      let v0 = get_uint8 pointer in
      let v1 = get_uint8 pointer in
      let v = (v0 lsl 8) lor v1 in
      if v land (1 lsl 15) <> 0 then
        (-1 land (lnot 0xffff)) lor v
      else
        v
  );
  write = (
    fun buffer value ->
      put_uint8 buffer ((value lsr 8) land 0xff);
      put_uint8 buffer (value land 0xff)
  );
}

let uint16 = int16

let int32 = {
  read = (
    fun pointer ->
      let v0 = get_uint8 pointer in
      let v1 = get_uint8 pointer in
      let v2 = get_uint8 pointer in
      let v3 = get_uint8 pointer in
      (v0 lsl 24) lor (v1 lsl 16) lor (v2 lsl 8) lor v3
  );
  write = (
    fun buffer value ->
      put_uint8 buffer ((value lsr 24) land 0xff);
      put_uint8 buffer ((value lsr 16) land 0xff);
      put_uint8 buffer ((value lsr 8) land 0xff);
      put_uint8 buffer (value land 0xff)
  );
}

let uint32 = int32

let boolean = {
  read = (fun pointer -> get_uint8 pointer <> 0);
  write = (fun buffer value -> put_uint8 buffer (if value then 1 else 0));
}

let string = {
  read = (
    fun pointer ->
      let index =
        try
          String.index_from pointer.buffer pointer.offset '\000'
        with Not_found ->
          String.length pointer.buffer
      in
      let offset = pointer.offset in
      pointer.offset <- index + 1;
      String.sub pointer.buffer offset (index - offset)
  );
  write = (
    fun buffer value ->
      Buffer.add_string buffer value;
      Buffer.add_char buffer '\x00'
  );
}

let array count conv = {
  read = (
    fun pointer ->
      Array.init count (fun i -> conv.read pointer)
  );
  write = (
    fun buffer value ->
      for i = 0 to count - 1 do
        conv.write buffer value.(i)
      done
  );
}

let read conv str =
  conv.read { buffer = str; offset = 0 }

let write conv value =
  let buf = Buffer.create 42 in
  conv.write buf value;
  Buffer.contents buf


let unit = {
  read = (fun pointer -> ());
  write = (fun buffer () -> ());
}

let seq2 c1 c2 = {
  read = (
    fun pointer ->
      let x1 = c1.read pointer in
      let x2 = c2.read pointer in
      (x1, x2)
  );
  write = (
    fun buffer (x1, x2) ->
      c1.write buffer x1;
      c2.write buffer x2
  );
}

let seq3 c1 c2 c3 = {
  read = (
    fun pointer ->
      let x1 = c1.read pointer in
      let x2 = c2.read pointer in
      let x3 = c3.read pointer in
      (x1, x2, x3)
  );
  write = (
    fun buffer (x1, x2, x3) ->
      c1.write buffer x1;
      c2.write buffer x2;
      c3.write buffer x3
  );
}

let seq4 c1 c2 c3 c4 = {
  read = (
    fun pointer ->
      let x1 = c1.read pointer in
      let x2 = c2.read pointer in
      let x3 = c3.read pointer in
      let x4 = c4.read pointer in
      (x1, x2, x3, x4)
  );
  write = (
    fun buffer (x1, x2, x3, x4) ->
      c1.write buffer x1;
      c2.write buffer x2;
      c3.write buffer x3;
      c4.write buffer x4
  );
}

let seq5 c1 c2 c3 c4 c5 = {
  read = (
    fun pointer ->
      let x1 = c1.read pointer in
      let x2 = c2.read pointer in
      let x3 = c3.read pointer in
      let x4 = c4.read pointer in
      let x5 = c5.read pointer in
      (x1, x2, x3, x4, x5)
  );
  write = (
    fun buffer (x1, x2, x3, x4, x5) ->
      c1.write buffer x1;
      c2.write buffer x2;
      c3.write buffer x3;
      c4.write buffer x4;
      c5.write buffer x5
  );
}

let seq6 c1 c2 c3 c4 c5 c6 = {
  read = (
    fun pointer ->
      let x1 = c1.read pointer in
      let x2 = c2.read pointer in
      let x3 = c3.read pointer in
      let x4 = c4.read pointer in
      let x5 = c5.read pointer in
      let x6 = c6.read pointer in
      (x1, x2, x3, x4, x5, x6)
  );
  write = (
    fun buffer (x1, x2, x3, x4, x5, x6) ->
      c1.write buffer x1;
      c2.write buffer x2;
      c3.write buffer x3;
      c4.write buffer x4;
      c5.write buffer x5;
      c6.write buffer x6
  );
}

let seq7 c1 c2 c3 c4 c5 c6 c7 = {
  read = (
    fun pointer ->
      let x1 = c1.read pointer in
      let x2 = c2.read pointer in
      let x3 = c3.read pointer in
      let x4 = c4.read pointer in
      let x5 = c5.read pointer in
      let x6 = c6.read pointer in
      let x7 = c7.read pointer in
      (x1, x2, x3, x4, x5, x6, x7)
  );
  write = (
    fun buffer (x1, x2, x3, x4, x5, x6, x7) ->
      c1.write buffer x1;
      c2.write buffer x2;
      c3.write buffer x3;
      c4.write buffer x4;
      c5.write buffer x5;
      c6.write buffer x6;
      c7.write buffer x7
  );
}

let seq8 c1 c2 c3 c4 c5 c6 c7 c8 = {
  read = (
    fun pointer ->
      let x1 = c1.read pointer in
      let x2 = c2.read pointer in
      let x3 = c3.read pointer in
      let x4 = c4.read pointer in
      let x5 = c5.read pointer in
      let x6 = c6.read pointer in
      let x7 = c7.read pointer in
      let x8 = c8.read pointer in
      (x1, x2, x3, x4, x5, x6, x7, x8)
  );
  write = (
    fun buffer (x1, x2, x3, x4, x5, x6, x7, x8) ->
      c1.write buffer x1;
      c2.write buffer x2;
      c3.write buffer x3;
      c4.write buffer x4;
      c5.write buffer x5;
      c6.write buffer x6;
      c7.write buffer x7;
      c8.write buffer x8
  );
}
