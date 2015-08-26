(** Headers *)

let prefix_small_block =         0x80
let prefix_small_int =           0x40
let prefix_small_string =        0x20

let code_int8 =                  0x00
let code_int16 =                 0x01
let code_int32 =                 0x02
let code_int64 =                 0x03
let code_shared8 =               0x04
let code_shared16 =              0x05
let code_shared32 =              0x06
let code_double_array32_little = 0x07
let code_block32 =               0x08
let code_string8 =               0x09
let code_string32 =              0x0A
let code_double_big =            0x0B
let code_double_little =         0x0C
let code_double_array8_big =     0x0D
let code_double_array8_little =  0x0E
let code_double_array32_big =    0x0F
let code_codepointer =           0x10
let code_infixpointer =          0x11
let code_custom =                0x12
let code_block64 =               0x13

type code_descr =
| CODE_INT8
| CODE_INT16
| CODE_INT32
| CODE_INT64
| CODE_SHARED8
| CODE_SHARED16
| CODE_SHARED32
| CODE_DOUBLE_ARRAY32_LITTLE
| CODE_BLOCK32
| CODE_STRING8
| CODE_STRING32
| CODE_DOUBLE_BIG
| CODE_DOUBLE_LITTLE
| CODE_DOUBLE_ARRAY8_BIG
| CODE_DOUBLE_ARRAY8_LITTLE
| CODE_DOUBLE_ARRAY32_BIG
| CODE_CODEPOINTER
| CODE_INFIXPOINTER
| CODE_CUSTOM
| CODE_BLOCK64

let code_max = 0x13

let magic_number = "\132\149\166\190"

(** Memory reification *)

type event =
| RInt of int
| RBlock of int * int (* tag × len *)
| RString of string
| RPointer of int
| RCode of int

type data =
| Int of int (* value *)
| Ptr of int (* pointer *)
| Atm of int (* tag *)
| Fun of int (* address *)

type obj =
| Struct of int * data array (* tag × data *)
| String of string

type header = {
  magic : string;
  length : int;
  size32 : int;
  size64 : int;
  objects : int
}

type 'a listener = {
  header : header -> 'a;
  event : event -> 'a -> 'a;
  close : 'a -> 'a;
}

(* let input_byte (s, off) =
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

module type Input =
sig
  type t
  val input_byte : t -> int
  val input_binary_int : t -> int
end

module type S =
sig
  type input
  val parse : input -> (data * obj array)
  val listen : input -> 'a listener -> 'a
end

module Make(M : Input) =
struct

open M

type input = M.t

let current_offset = ref 0

let input_byte chan =
  let () = incr current_offset in
  input_byte chan

let input_binary_int chan =
  let () = current_offset := !current_offset + 4 in
  input_binary_int chan

let input_char chan = Char.chr (input_byte chan)

let parse_header chan =
  let magic = String.create 4 in
  let () = for i = 0 to 3 do magic.[i] <- input_char chan done in
  let length = input_binary_int chan in
  let objects = input_binary_int chan in
  let size32 = input_binary_int chan in
  let size64 = input_binary_int chan in
  { magic; length; size32; size64; objects; }

let input_int8s chan =
  let i = input_byte chan in
  if i land 0x80 = 0
    then i
    else i lor ((-1) lsl 8)

let input_int8u = input_byte

let input_int16s chan =
  let i = input_byte chan in
  let j = input_byte chan in
  let ans = (i lsl 8) lor j in
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
  let i = input_byte chan in
  let j = input_byte chan in
  let k = input_byte chan in
  let l = input_byte chan in
  let m = input_byte chan in
  let n = input_byte chan in
  let o = input_byte chan in
  let p = input_byte chan in
  let ans =
    (i lsl 56) lor (j lsl 48) lor (k lsl 40) lor (l lsl 32) lor
    (m lsl 24) lor (n lsl 16) lor (o lsl 8) lor p
  in
  if i land 0x80 = 0
    then ans
    else ans lor ((-1) lsl 63)

let input_int64u chan =
  let i = input_byte chan in
  let j = input_byte chan in
  let k = input_byte chan in
  let l = input_byte chan in
  let m = input_byte chan in
  let n = input_byte chan in
  let o = input_byte chan in
  let p = input_byte chan in
  (i lsl 56) lor (j lsl 48) lor (k lsl 40) lor (l lsl 32) lor
  (m lsl 24) lor (n lsl 16) lor (o lsl 8) lor p

let input_header32 chan =
  let i = input_byte chan in
  let j = input_byte chan in
  let k = input_byte chan in
  let l = input_byte chan in
  let tag = l in
  let len = (i lsl 14) lor (j lsl 6) lor (k lsr 2) in
  (tag, len)

let input_header64 chan =
  let i = input_byte chan in
  let j = input_byte chan in
  let k = input_byte chan in
  let l = input_byte chan in
  let m = input_byte chan in
  let n = input_byte chan in
  let o = input_byte chan in
  let p = input_byte chan in
  let tag = p in
  let len =
    (i lsl 46) lor (j lsl 38) lor (k lsl 30) lor (l lsl 22) lor
    (m lsl 14) lor (n lsl 6) lor (o lsr 2)
  in
  (tag, len)

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
  else if data > code_max then
    assert false
  else match (Obj.magic data) with
  | CODE_INT8 ->
    RInt (input_int8s chan)
  | CODE_INT16 ->
    RInt (input_int16s chan)
  | CODE_INT32 ->
    RInt (input_int32s chan)
  | CODE_INT64 ->
    RInt (input_int64s chan)
  | CODE_SHARED8 ->
    RPointer (input_int8u chan)
  | CODE_SHARED16 ->
    RPointer (input_int16u chan)
  | CODE_SHARED32 ->
    RPointer (input_int32u chan)
  | CODE_BLOCK32 ->
    let (tag, len) = input_header32 chan in
    RBlock (tag, len)
  | CODE_BLOCK64 ->
    let (tag, len) = input_header64 chan in
    RBlock (tag, len)
  | CODE_STRING8 ->
    let len = input_int8u chan in
    RString (input_string len chan)
  | CODE_STRING32 ->
    let len = input_int32u chan in
    RString (input_string len chan)
  | CODE_CODEPOINTER ->
    let addr = input_int32u chan in
    for i = 0 to 15 do ignore (input_byte chan); done;
    RCode addr
  | CODE_DOUBLE_ARRAY32_LITTLE
  | CODE_DOUBLE_BIG
  | CODE_DOUBLE_LITTLE
  | CODE_DOUBLE_ARRAY8_BIG
  | CODE_DOUBLE_ARRAY8_LITTLE
  | CODE_DOUBLE_ARRAY32_BIG
  | CODE_INFIXPOINTER
  | CODE_CUSTOM ->
    Printf.eprintf "Unknown code %04x\n%!" data; assert false

let parse chan =
  let header = parse_header chan in
  let () = assert (header.magic = magic_number) in
  let memory = Array.make header.objects (Struct ((-1), [||])) in
  let current_object = ref 0 in
  let fill_obj = function
  | RPointer n ->
    let data = Ptr (!current_object - n) in
    data, None
  | RInt n ->
    let data = Int n in
    data, None
  | RString s ->
    let data = Ptr !current_object in
    let () = memory.(!current_object) <- String s in
    let () = incr current_object in
    data, None
  | RBlock (tag, 0) ->
    (* Atoms are never shared *)
    let data = Atm tag in
    data, None
  | RBlock (tag, len) ->
    let data = Ptr !current_object in
    let nblock = Array.make len (Atm (-1)) in
    let () = memory.(!current_object) <- Struct (tag, nblock) in
    let () = incr current_object in
    data, Some nblock
  | RCode addr ->
    let data = Fun addr in
    data, None
  in

  let rec fill block off accu =
    if Array.length block = off then
      match accu with
      | [] -> ()
      | (block, off) :: accu -> fill block off accu
    else
      let data, nobj = fill_obj (parse_object chan) in
      let () = block.(off) <- data in
      let block, off, accu = match nobj with
      | None -> block, succ off, accu
      | Some nblock -> nblock, 0, ((block, succ off) :: accu)
      in
      fill block off accu
  in
  let ans = [|Atm (-1)|] in
  let () = fill ans 0 [] in
  (ans.(0), memory)

  let listen chan listener =
    let header = parse_header chan in
    let () = current_offset := 0 in
    let () = assert (header.magic = magic_number) in
    let accu = listener.header header in
    let count = header.objects in
    let rec run accu =
      if !current_offset = header.length then listener.close accu
      else
        let ev = parse_object chan in
        let accu = listener.event ev accu in
        run accu
    in
    run accu

(*let dump chan =
  let magic = input_binary_int chan in
  let light = parse chan in
  let digest = parse chan in
  let table = parse chan in
  (magic, light, digest, table)

let () =
  let chan = open_in Sys.argv.(1) in
  let _ = dump chan in
  ()*)

end

module IChannel =
struct
  type t = in_channel
  let input_byte = Pervasives.input_byte
  let input_binary_int = Pervasives.input_binary_int
end

module IString =
struct
  type t = (string * int ref)

  let input_byte (s, off) =
    let ans = Char.code (s.[!off]) in
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

end

module PChannel = Make(IChannel)
module PString = Make(IString)

let parse_channel = PChannel.parse
let parse_string s = PString.parse (s, ref 0)

let listen_channel = PChannel.listen
let listen_string s l = PString.listen (s, ref 0) l
