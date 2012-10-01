module IntOrd =
struct
  type t = int
  let compare = Pervasives.compare
end

(*module StringOrd =
struct
  type t = string
  let compare = Pervasives.compare
end
*)
module IntMap = Map.Make(IntOrd)
module IntSet = Set.Make(IntOrd)

module IntSetOrd =
struct
  type t = IntSet.t
  let compare = IntSet.compare
end

module IntSetSet = Set.Make(IntSetOrd)
module IntSetMap = Map.Make(IntSetOrd)

(*
module StringMap = Map.Make(StringOrd)
module StringSet = Set.Make(StringOrd)*)

module Hash =
struct
  type t = Obj.t
  let equal = (==)
  let hash = Hashtbl.hash
end

module Env = Hashtbl.Make(Hash)

type data =
| Int of int
| Ptr of int
| Ext of Obj.t

type obj =
| Array of int * data array
| String of string
| Double of float
| Closure
| Lazy
| Abstract of int

let pr_data chan = function
| Int i -> Printf.fprintf chan "i:%i" i
| Ptr ptr -> Printf.fprintf chan "p:0x%08x" ptr
| Ext obj -> Printf.fprintf chan "x:0x%08x" (Obj.magic obj)

let pr_obj chan = function
| Array (i, [|x|]) ->
  Printf.fprintf chan "%i[%a]" i pr_data x
| Array (i, v) ->
  let len = Array.length v in
  let rec pr_list chan i =
    if len <= i then ()
    else if pred len = i then pr_data chan v.(i)
    else
      Printf.fprintf chan "%a %a" pr_data v.(i) pr_list (succ i)
  in
  Printf.fprintf chan "%i[%a]" i pr_list 0
| String s -> Printf.fprintf chan "\"%s\"" (String.escaped s)
| Double f -> Printf.fprintf chan "float"
| Closure -> Printf.fprintf chan "closure"
| Lazy -> Printf.fprintf chan "lazy"
| Abstract t -> Printf.fprintf chan "abstract:%i" t

let pr_mem chan obj mem =
  let iter id obj = Printf.fprintf chan "0x%08x -> %a\n" id pr_obj obj in
  IntMap.iter iter mem

let (genid, reset) =
  let i = ref 0 in (fun () ->
    let ans = !i in
    let () = incr i in
    ans), (fun () -> i := 0)

let rec represent env mem obj =
  if Obj.is_int obj then Int (Obj.obj obj)
  else if Obj.is_block obj then
    try Ptr (Env.find env obj)
    with Not_found ->
      let id = genid () in
      let () = Env.add env obj id in
      let () = register env mem obj id in
      Ptr id
  else Ext (Obj.obj obj)

and register env mem obj id =
  let tag = Obj.tag obj in
  if tag < Obj.no_scan_tag then
    if tag = Obj.closure_tag then
      Hashtbl.add mem id Closure
    else if tag = Obj.lazy_tag then
      Hashtbl.add mem id Lazy
    else
      let s = Obj.size obj in
      let init i = represent env mem (Obj.field obj i) in
      let v = Array.init s init in
      Hashtbl.add mem id (Array (tag, v))
  else
    if tag = Obj.string_tag then
      let s : string = Obj.obj obj in
      Hashtbl.add mem id (String s)
    else if tag = Obj.double_tag then
      let f : float = Obj.obj obj in
      Hashtbl.add mem id (Double f)
    else
      Hashtbl.add mem id (Abstract tag)

let dump obj =
  let () = reset () in
  let env = Env.create 97 in
  let mem = Hashtbl.create 97 in
  let ans = represent env mem (Obj.repr obj) in
  let fold id v accu = IntMap.add id v accu in
  let mem = Hashtbl.fold fold mem IntMap.empty in
  (ans, mem)

let undump obj mem =
  let made = ref IntMap.empty in
  let rec undump_aux = function
  | Int i -> Obj.repr i
  | Ext obj -> obj
  | Ptr ptr ->
    try IntMap.find ptr !made
    with Not_found ->
      (* The object has not been created yet *)
      match IntMap.find ptr mem with
      | Array (tag, a) ->
        let obj = Obj.new_block tag (Array.length a) in
        (* register the new object *)
        let () = made := IntMap.add ptr obj !made in
        let len = Array.length a in
        let i = ref 0 in
        while !i < len do
          let arg = undump_aux a.(!i) in
          Obj.set_field obj !i arg;
          incr i
        done;
        obj
      | String s ->
        let obj = Obj.repr (String.copy s) in
        (* register the new object *)
        let () = made := IntMap.add ptr obj !made in
        obj
      | Double f ->
        let obj = Obj.repr f in
        (* register the new object *)
        let () = made := IntMap.add ptr obj !made in
        obj
      | Closure | Lazy | Abstract _ -> invalid_arg "undump"
  in
  undump_aux obj

let pr_dot_data chan = function
| Int i -> Printf.fprintf chan "%i" i
| Ptr _ -> Printf.fprintf chan "_"
| Ext obj -> Printf.fprintf chan "0x%08x" (Obj.magic obj)

let pr_dot_obj ptr chan = function
| Array (i, [||]) ->
(* Printf.eprintf "addr:%x\n" ptr; *)
  Printf.fprintf chan "[label=\"struct_%08x::tag_%i\", fillcolor=\"red\", style=\"filled\"]" ptr i
| Array (i, v) ->
  let pr_array chan v =
    let len = Array.length v in
    for i = 0 to len - 2 do
      Printf.fprintf chan "<f%i>%a|" i pr_dot_data v.(i)
    done;
    Printf.fprintf chan "<f%i>%a" (pred len) pr_dot_data v.(pred len)
  in
  Printf.fprintf chan "[label=\"struct:%08x::%i|%a\"]" ptr i pr_array v
| String s ->
  Printf.fprintf chan "[label=\"string:%08x\", fillcolor=\"green\", style=\"filled\"]" ptr
| Double f -> ()
| Closure -> ()
| Lazy -> ()
| Abstract t -> ()

let pr_dot_link test ptr chan = function
| Array (i, [||]) -> ()
| Array (i, v) ->
  let len = Array.length v in
  for i = 0 to len - 1 do
    match v.(i) with
    | Ptr dst ->
      if test dst then
        Printf.fprintf chan "struct_%08x:f%i -> struct_%08x;\n" ptr i dst
    | _ -> ()
  done
| _ -> ()

type transition =
| StringT of string
| TagT of int
| PFieldT of int (* field number *)
| IFieldT of int * int (* field number * int value *)

module TransitionOrd =
struct
  type t = transition
  let compare = Pervasives.compare
end

module HC = Hopcroft.Make(TransitionOrd)

let normalize obj mem cl =
  let size = IntMap.cardinal mem in
  let assoc = Array.make size (-1) in
  let iter cli elts = List.iter (fun i -> assoc.(i) <- cli) elts in
  let () = Array.iteri iter cl in
  let canonical content = match content with
  | Int n -> content
  | Ptr p -> Ptr assoc.(p)
  | _ -> assert false
  in
  let fold (idx, accu) cli =
    (** Choose an element *)
    let repr = List.hd cli in
    let data = match IntMap.find repr mem with
    | Array (tag, value) ->
      Array (tag, Array.map canonical value)
    | String s -> String s
    | _ -> assert false
    in
    (succ idx, IntMap.add idx data accu)
  in
  let (_, mem) = Array.fold_left fold (0, IntMap.empty) cl in
  (canonical obj, mem)

let reduce obj mem =
  let size = IntMap.cardinal mem in
  let transitions = ref [] in
  let push lbl src dst =
    let t = { Hopcroft.lbl = lbl; src = src; dst = dst } in
    transitions := t :: !transitions
  in
  let iter ptr = function
  | Array (tag, value) ->
    let () = push (TagT tag) ptr ptr in
    let iter i = function
    | Int n -> push (IFieldT (i, n)) ptr ptr
    | Ptr q -> push (PFieldT i) ptr q
    | _ -> invalid_arg "reduce"
    in
    Array.iteri iter value
  | String s ->
    push (StringT s) ptr ptr
  | _ -> invalid_arg "reduce"
  in
  let () = IntMap.iter iter mem in
  let automaton = {
    Hopcroft.states = size;
    transitions = Array.of_list !transitions;
    final_states = [||];
  } in
  let reduced = HC.reduce automaton in
  normalize obj mem reduced

let filter mem dest =
  (* compute reverse pointers *)
  let fold ptr obj accu = match obj with
  | Array (i, v) ->
    let len = Array.length v in
    let rec exist accu i =
      if len <= i then accu
      else begin match v.(i) with
      | Ptr dst ->
        let old = try IntMap.find dst accu with Not_found -> IntSet.empty in
        exist (IntMap.add dst (IntSet.add ptr old) accu) (succ i)
      | _ -> exist accu (succ i)
      end
    in
    exist accu 0
  | _ -> accu
  in
  let rev_ptr = IntMap.fold fold mem IntMap.empty in
  (** reverse closure *)
  let step visited todo =
    let nvisited = IntSet.union visited todo in
    let fold elt accu =
      let neighbours = try IntMap.find elt rev_ptr with Not_found -> IntSet.empty in
      let neighbours = IntSet.filter (fun e -> not (IntSet.mem e nvisited)) neighbours in
      IntSet.union neighbours accu
    in
    let neighbours = IntSet.fold fold todo IntSet.empty in
    (nvisited, neighbours)
  in
  let rec closure visited todo =
    let (nv, nt) = step visited todo in
    if IntSet.is_empty todo then nv
    else closure nv nt
  in
  let from_i = closure IntSet.empty (IntSet.singleton dest) in
  IntMap.filter (fun p _ -> IntSet.mem p from_i) mem

let pr_dot_mem chan obj mem =
  let iter ptr obj =
    Printf.fprintf chan "struct_%08x%a;\n" ptr (pr_dot_obj ptr) obj;
    pr_dot_link (fun ptr -> IntMap.mem ptr mem) ptr chan obj;
    Printf.fprintf chan "\n"
  in
  Printf.fprintf chan "digraph G {\n";
  Printf.fprintf chan "node[shape=\"record\"]\n";
  IntMap.iter iter mem;
  Printf.fprintf chan "\n}\n"

let parse chan =
  let magic = String.create 4 in
  let () = for i = 0 to 3 do magic.[i] <- input_char chan done in
  let length = input_binary_int chan in
  let objects = input_binary_int chan in
  let size32 = input_binary_int chan in
  let size64 = input_binary_int chan in
  for i = 0 to pred length do
    ignore (input_char chan);
  done;
  (magic, length, size32, size64, objects)

let main () =
  let file = Sys.argv.(1) in
  let () = Printf.eprintf "unmarshalling...\n%!" in
  let chan = open_in file in
  (* magic number *)
  let _ = input_binary_int chan in
  (* light lib *)
  let (_, s, _, _, obj) = parse chan in
  let () = Printf.eprintf "lib: %i bytes, %i objects\n%!" s obj in
  (* digest *)
  let _ = parse chan in
  (* full lib *)
  let (_, s, _, _, obj) = parse chan in
  let () = Printf.eprintf "table: %i bytes, %i objects\n%!" s obj in
  let () = close_in chan in
  let () = Printf.eprintf "dumping...\n%!" in
(*   let obj, mem = dump obj in *)
(*   let () = Printf.eprintf "memsize: %i\n%!" (IntMap.cardinal mem) in *)
(*   let obj, mem = reduce obj mem in *)
(*   let () = Printf.eprintf "memsize: %i\n%!" (IntMap.cardinal mem) in *)
(*   let _ = undump obj mem in *)
(*   pr_mem stdout obj mem *)
(*   pr_dot_mem stdout obj (filter mem 0x00) *)
(*   pr_dot_mem stdout obj (filter mem 0x71b) *)
  ()

let () = main ()
