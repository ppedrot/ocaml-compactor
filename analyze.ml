let prefix_small_block = 0x80
let prefix_small_int = 0x40
let prefix_small_string = 0x20

let code_int8 = 0x00
let code_int16 = 0x01
let code_int32 = 0x02
let code_int64 = 0x03
let code_shared8 = 0x04
let code_shared16 = 0x05
let code_shared32 = 0x06
let code_block32 = 0x8
let code_block64 = 0x13
let code_string8 = 0x9
let code_string32 = 0xA

(*let input_byte (s, off) =
  let ans = Char.code (s.[!off]) in
  let () = incr off in
  ans

let input_char (s, off) =
  let ans = s.[!off] in
  let () = incr off in
  ans

let input_binary_int chan =
  let i = input_byte chan in
  let j = input_byte chan in
  let k = input_byte chan in
  let l = input_byte chan in
  let ans = (i lsl 24) lor (j lsl 16) lor (k lsl 8) lor l in
  if i land 0x80 = 0
    then ans
    else ans lor ((-1) lsl 31)

let of_string s = (s, ref 0)*)

let current_offset = ref 0

let input_byte chan =
  let () = incr current_offset in
  input_byte chan

let input_binary_int chan =
  let () = current_offset := !current_offset + 4 in
  input_binary_int chan

let input_char chan =
  let () = incr current_offset in
  input_char chan

let parse_header chan =
  let () = current_offset := 0 in
  let magic = String.create 4 in
  let () = for i = 0 to 3 do magic.[i] <- input_char chan done in
  let length = input_binary_int chan in
  let objects = input_binary_int chan in
  let size32 = input_binary_int chan in
  let size64 = input_binary_int chan in
  (magic, length, size32, size64, objects)

type repr =
| RInt of int
| RBlock of (int * int) (* tag × len *)
| RString of string
| RPointer of int

type data =
| Int of int
| Ptr of int
| Atom of int (* tag *)

type obj =
| Struct of int * data array (* tag × data *)
| String of string

let input_int8s chan =
  let i = input_byte chan in
  if i land 0x80 = 0
    then i
    else i lor ((-1) lsl 8)

let input_int8u = input_byte

let input_int16s chan =
  let i = input_byte chan in
  let j = input_byte chan in
  let ans = (i lsl 8) land j in
  if i land 0x80 = 0
    then ans
    else ans lor ((-1) lsl 16)

let input_int16u chan =
  let i = input_byte chan in
  let j = input_byte chan in
  (i lsl 8) lor j

let input_int32s chan =
  let i = input_byte chan in
  let j = input_byte chan in
  let k = input_byte chan in
  let l = input_byte chan in
  let ans = (i lsl 24) lor (j lsl 16) lor (k lsl 8) lor l in
  if i land 0x80 = 0
    then ans
    else ans lor ((-1) lsl 31)

let input_int32u chan =
  let i = input_byte chan in
  let j = input_byte chan in
  let k = input_byte chan in
  let l = input_byte chan in
  (i lsl 24) lor (j lsl 16) lor (k lsl 8) lor l

let input_int64s chan =
(*  let i = input_byte chan in
  let j = input_byte chan in
  let k = input_byte chan in
  let l = input_byte chan in
  let m = input_byte chan in
  let n = input_byte chan in
  let o = input_byte chan in
  let p = input_byte chan in*)
  assert false

let input_int64u chan =
(*  let i = input_byte chan in
  let j = input_byte chan in
  let k = input_byte chan in
  let l = input_byte chan in
  let m = input_byte chan in
  let n = input_byte chan in
  let o = input_byte chan in
  let p = input_byte chan in*)
  assert false

let input_header32 chan =
  let i = input_byte chan in
  let j = input_byte chan in
  let k = input_byte chan in
  let l = input_byte chan in
  let tag = l in
  let len = (i lsl 14) lor (j lsl 6) lor (k lsr 2) in
  (tag, len)

let input_header64 chan =
  assert false

let input_string len chan =
  let ans = String.create len in
  for i = 0 to pred len do
    ans.[i] <- input_char chan;
  done;
  ans

let parse_object chan =
  let data = input_byte chan in
  if prefix_small_block <= data then
    let tag = data land 0x0F in
    let len = (data lsr 4) land 0x07 in
    RBlock (tag, len)
  else if prefix_small_int <= data then
    RInt (data land 0x3F)
  else if prefix_small_string <= data then
    let len = data land 0x1F in
    RString (input_string len chan)
  else if data = code_int8 then
    RInt (input_int8s chan)
  else if data = code_int16 then
    RInt (input_int16s chan)
  else if data = code_int32 then
    RInt (input_int32s chan)
  else if data = code_int64 then
    RInt (input_int64s chan)
  else if data = code_shared8 then
    RPointer (input_int8u chan)
  else if data = code_shared16 then
    RPointer (input_int16u chan)
  else if data = code_shared32 then
    RPointer (input_int32u chan)
  else if data = code_block32 then
    RBlock (input_header32 chan)
  else if data = code_block64 then
    RBlock (input_header64 chan)
  else if data = code_string8 then
    let len = input_int8u chan in
    RString (input_string len chan)
  else if data = code_string32 then
    let len = input_int32u chan in
    RString (input_string len chan)
  else
    (Printf.eprintf "Unknown code %04x\n%!" data; assert false)

let parse_data len chan =
  let () = current_offset := 0 in
  let rec parse accu =
    if len <= !current_offset then accu
    else parse (parse_object chan :: accu)
  in
  parse []

let parse chan =
  let (_, len, _, _, _) = parse_header chan in
  parse_data len chan

let rec take value off len l =
  if len <= off then l
  else match l with
  | [] -> assert false
  | x :: l ->
    value.(off) <- x;
    take value (succ off) len l

let parse chan =
  let (_, len, _, _, size) = parse_header chan in
  let stream = parse_data len chan in
  let memory = Array.make size (Struct ((-1), [||])) in
  let current_object = ref (pred size) in
  let rec fill accu = function
  | [] -> ()
  | RPointer n :: mem ->
    let data = Ptr (!current_object - n + 1) in
    fill (data :: accu) mem
  | RInt n :: mem ->
    let data = Int n in
    fill (data :: accu) mem
  | RString s :: mem ->
    let data = Ptr !current_object in
    let () = memory.(!current_object) <- String s in
    let () = decr current_object in
    fill (data :: accu) mem
  | RBlock (tag, 0) :: mem ->
    (* Atoms are never shared *)
    let data = Atom tag in
    fill (data :: accu) mem
  | RBlock (tag, len) :: mem ->
    let data = Ptr !current_object in
    let block = Array.make len (Atom (-1)) in
    let () = memory.(!current_object) <- Struct (tag, block) in
    let () = decr current_object in
    let accu = take block 0 len accu in
    fill (data :: accu) mem
  in
  let () = fill [] stream in
  memory

let dump chan =
  let magic = input_binary_int chan in
  let light = parse chan in
  let digest = parse chan in
  let table = parse chan in
  (magic, light, digest, table)

let () =
  let chan = open_in Sys.argv.(1) in
  let _ = dump chan in
  ()
