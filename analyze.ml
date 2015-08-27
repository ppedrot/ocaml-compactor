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
  iheader : header -> 'a;
  ievent : event -> 'a -> 'a;
  iclose : 'a -> 'a;
}

type echoer = {
  oevent : event -> unit;
  oclose : unit -> header;
}

module type Input =
sig
  type t
  val input_byte : t -> int
  val input_binary_int : t -> int
end

module type Output =
sig
  type t
  val output_byte : t -> int -> unit
  val output_binary_int : t -> int -> unit
  val pos : t -> int
  val seek : t -> int -> unit
end

module type IS =
sig
  type input
  val parse : input -> (data * obj array)
  val listen : input -> 'a listener -> 'a
end

module type OS =
sig
  type output
  val echo : output -> echoer
end

module IMake(M : Input) =
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
    let accu = listener.iheader header in
    let count = header.objects in
    let rec run accu =
      if !current_offset = header.length then listener.iclose accu
      else
        let ev = parse_object chan in
        let accu = listener.ievent ev accu in
        run accu
    in
    run accu

end

module OMake(M : Output) =
struct

open M

type output = M.t

let current_offset = ref 0

let output_byte chan i =
  let () = incr current_offset in
  output_byte chan i

let output_binary_int chan i =
  let () = current_offset := !current_offset + 4 in
  output_binary_int chan i

let output_char chan c = output_byte chan (Char.code c)

let print_header chan header =
  for i = 0 to 3 do output_char chan header.magic.[i] done;
  output_binary_int chan header.length;
  output_binary_int chan header.objects;
  output_binary_int chan header.size32;
  output_binary_int chan header.size64

let output_int8s chan n =
  let i = (n land 0x7F) lor (if n < 0 then 0x80 else 0x00) in
  output_byte chan i

let output_int8u = output_byte

let output_int16s chan n =
  let i = ((n lsr 8) land 0x7F) lor (if n < 0 then 0x80 else 0x00) in
  let j = n land 0xFF in
  output_byte chan i;
  output_byte chan j

let output_int16u chan n =
  let i = (n lsr 8) land 0xFF in
  let j = n land 0xFF in
  output_byte chan i;
  output_byte chan j

let output_int32s chan n =
  let i = ((n lsr 24) land 0x7F) lor (if n < 0 then 0x80 else 0x00) in
  let j = (n lsr 16) land 0xFF in
  let k = (n lsr 8) land 0xFF in
  let l = n land 0xFF in
  output_byte chan i;
  output_byte chan j;
  output_byte chan k;
  output_byte chan l

let output_int32u chan n =
  let i = (n lsr 24) land 0xFF in
  let j = (n lsr 16) land 0xFF in
  let k = (n lsr 8) land 0xFF in
  let l = n land 0xFF in
  output_byte chan i;
  output_byte chan j;
  output_byte chan k;
  output_byte chan l

let output_int64s chan e =
  let i = ((e lsr 56) land 0x7F) lor (if e < 0 then 0x80 else 0x00) in
  let j = (e lsr 48) land 0xFF in
  let k = (e lsr 40) land 0xFF in
  let l = (e lsr 32) land 0xFF in
  let m = (e lsr 24) land 0xFF in
  let n = (e lsr 16) land 0xFF in
  let o = (e lsr 8) land 0xFF in
  let p = e land 0xFF in
  output_byte chan i;
  output_byte chan j;
  output_byte chan k;
  output_byte chan l;
  output_byte chan m;
  output_byte chan n;
  output_byte chan o;
  output_byte chan p

let output_int64u chan e =
  let i = (e lsr 56) land 0xFF in
  let j = (e lsr 48) land 0xFF in
  let k = (e lsr 40) land 0xFF in
  let l = (e lsr 32) land 0xFF in
  let m = (e lsr 24) land 0xFF in
  let n = (e lsr 16) land 0xFF in
  let o = (e lsr 8) land 0xFF in
  let p = e land 0xFF in
  output_byte chan i;
  output_byte chan j;
  output_byte chan k;
  output_byte chan l;
  output_byte chan m;
  output_byte chan n;
  output_byte chan o;
  output_byte chan p

let output_header32 chan tag len =
  output_byte chan (len lsr 14);
  output_byte chan (len lsr 6);
  output_byte chan (len lsl 2);
  output_byte chan tag

let output_header64 chan tag len =
  output_byte chan (len lsr 46);
  output_byte chan (len lsr 38);
  output_byte chan (len lsr 30);
  output_byte chan (len lsr 22);
  output_byte chan (len lsr 14);
  output_byte chan (len lsr 6);
  output_byte chan (len lsl 2);
  output_byte chan tag

let output_string chan s =
  for i = 0 to String.length s - 1 do
    output_char chan s.[i];
  done

let fits n bound = - bound <= n && n < bound

let output_event chan = function
| RInt n ->
  if 0 <= n && n < prefix_small_int then
    output_byte chan (prefix_small_int lor n)
  else if fits n 0x80 then begin
    output_byte chan code_int8;
    output_int8s chan n
  end else if fits n 0x8000 then begin
    output_byte chan code_int16;
    output_int16s chan n
  end else if Sys.word_size = 32 || fits n 0x80000000 then begin
    output_byte chan code_int32;
    output_int32s chan n
  end else begin
    output_byte chan code_int64;
    output_int64s chan n
  end
| RBlock (tag, len) ->
  if tag <= 0x0F && len <= 0x07 then
    output_byte chan (prefix_small_block lor tag lor (len lsl 4))
  else if len <= 0x3FFFFFFF then begin
    output_byte chan code_block32;
    output_header32 chan tag len
  end else begin
    output_byte chan code_block64;
    output_header64 chan tag len
  end
| RString s ->
  let len = String.length s in
  if len < prefix_small_string then begin
    output_byte chan (prefix_small_string lor len);
    output_string chan s
  end else if len <= 0xFF then begin
    output_byte chan code_string8;
    output_int8u chan len;
    output_string chan s
  end else begin
    output_byte chan code_string32;
    output_int32u chan len;
    output_string chan s
  end
| RPointer ptr ->
  if ptr <= 0xFF then begin
    output_byte chan code_shared8;
    output_int8u chan ptr
  end else if ptr <= 0xFFFF then begin
    output_byte chan code_shared16;
    output_int16u chan ptr
  end else begin
    output_byte chan code_shared32;
    output_int32u chan ptr
  end
| RCode addr -> assert false

let get_size32 = function
| RInt _ -> 0
| RBlock (_, 0) -> 0
| RBlock (_, len) -> 1 + len
| RString s -> 2 + (String.length s) / 4
| RPointer _ -> 0
| RCode _ -> assert false

let get_size64 = function
| RInt _ -> 0
| RBlock (_, 0) -> 0
| RBlock (_, len) -> 1 + len
| RString s -> 2 + (String.length s) / 8
| RPointer _ -> 0
| RCode _ -> assert false

let get_objects = function
| RBlock (_, 0) | RPointer _ | RCode _ | RInt _ -> 0
| RBlock (_, _) | RString _ -> 1

let echo chan =
  let pos = M.pos chan in
  let objects = ref 0 in
  let size32 = ref 0 in
  let size64 = ref 0 in
  let oevent event =
    output_event chan event;
    size32 := get_size32 event + !size32;
    size64 := get_size64 event + !size64;
    objects := get_objects event + !objects;
  in
  let oclose () =
    let header = {
      magic = magic_number;
      objects = !objects;
      length = !current_offset;
      size32 = !size32;
      size64 = !size64;
    } in
    (** Outputs the header in the reserved room *)
    let npos = M.pos chan in
    M.seek chan pos;
    for i = 0 to 3 do output_byte chan (Char.code header.magic.[i]) done;
    output_binary_int chan header.length;
    output_binary_int chan header.objects;
    output_binary_int chan header.size32;
    output_binary_int chan header.size64;
    let () = M.seek chan npos in
    header
  in
  (** Output temporary placeholder bytes for the header *)
  for i = 0 to 4 do output_binary_int chan (-1) done;
  current_offset := 0;
  { oevent; oclose; }

end

module IChannel =
struct
  type t = in_channel
  let input_byte = Pervasives.input_byte
  let input_binary_int = Pervasives.input_binary_int
end

module OChannel =
struct
  type t = out_channel
  let output_byte = Pervasives.output_byte
  let output_binary_int = Pervasives.output_binary_int
  let seek = Pervasives.seek_out
  let pos = Pervasives.pos_out
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

module IPChannel = IMake(IChannel)
module IPString = IMake(IString)

module OPChannel = OMake(OChannel)

let parse_channel = IPChannel.parse
let parse_string s = IPString.parse (s, ref 0)

let listen_channel = IPChannel.listen
let listen_string s l = IPString.listen (s, ref 0) l

let echo_channel = OPChannel.echo
