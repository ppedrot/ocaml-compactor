open Analyze

type transition =
| AtomT of int * int (* field number * tag *)
| PFieldT of int (* field number *)
| IFieldT of int * int (* field number * int value *)

module TransitionOrd =
struct
  type t = transition

  let int_compare (x : int) y = Pervasives.compare x y

  let compare t1 t2 = match t1, t2 with
  | AtomT (n1, t1), AtomT (n2, t2) ->
    let c = int_compare n1 n2 in
    if c = 0 then int_compare t1 t2 else c
  | PFieldT n1, PFieldT n2 -> int_compare n1 n2
  | IFieldT (n1, i1), IFieldT (n2, i2) ->
    let c = int_compare n1 n2 in
    if c = 0 then int_compare i1 i2 else c

  | AtomT (_, _), (PFieldT _ | IFieldT (_, _)) -> -1
  | PFieldT _, (IFieldT (_, _)) -> -1

  | PFieldT _, (AtomT (_, _)) -> 1
  | IFieldT (_, _), (AtomT (_, _) | PFieldT _) -> 1

end

module HC = Hopcroft.Make(TransitionOrd)
module StringMap = Map.Make(String)

let normalize obj mem cl =
  let open HC in
  (** Initialize the new memory with dummy values *)
  let compact_mem = Array.make (SPartition.length cl) (Struct (-1, [||])) in
  let canonical content = match content with
  | Int _ | Atm _ -> content
  | Ptr p ->
    let repr = SPartition.partition p cl in
    Ptr (SPartition.represent repr)
  | Fun _ -> assert false
  in
  (** Fill the new memory with canonical names *)
  let iter set =
    let idx = SPartition.represent set in
    (** Choose an element *)
    let repr = SPartition.choose set cl in
    let data = match mem.(repr) with
    | Struct (tag, value) -> Struct (tag, Array.map canonical value)
    | String s -> String s
    in
    compact_mem.(idx) <- data
  in
  let () = SPartition.iter_all iter cl in
  (** Return canonical entry point and compacted memory *)
  (canonical obj, compact_mem)

let normalize_obj obj mem cl =
  let open HC in
  (** Initialize the new memory with dummy values *)
  let compact_mem = Array.make (SPartition.length cl) (Obj.repr 0) in
  let canonical content = match content with
  | Int i -> Obj.repr i
  | Atm tag -> Obj.new_block tag 0
  | Ptr p ->
    let repr = SPartition.partition p cl in
    compact_mem.(SPartition.represent repr)
  | Fun _ -> assert false
  in
  (** Fill the memory in two passes, to ensure we get the pointers right: first
      construct the structures. *)
  let iter set =
    let idx = SPartition.represent set in
    (** Choose an element *)
    let repr = SPartition.choose set cl in
    let data = match mem.(repr) with
    | Struct (tag, value) ->
      Obj.new_block tag (Array.length value)
    | String s -> Obj.repr s
    in
    compact_mem.(idx) <- data
  in
  let () = SPartition.iter_all iter cl in
  (** Then set the contents of the structures *)
  let iter set =
    let repr = SPartition.choose set cl in
    match mem.(repr) with
    | Struct (tag, value) ->
      let idx = SPartition.represent set in
      let obj = compact_mem.(idx) in
      for i = 0 to pred (Array.length value) do
        Obj.set_field obj i (canonical value.(i))
      done
    | String _ -> ()
  in
  let () = SPartition.iter_all iter cl in
  canonical obj

let to_automaton obj mem =
  (** Create the automaton *)
  let size = Array.length mem in
  let transitions = ref HC.TMap.empty in
  let tags = Array.make 256 [] in
  let strings = ref StringMap.empty in
  let push lbl src dst =
    let t = { HC.src = src; dst = dst } in
    let trans = try HC.TMap.find lbl !transitions with Not_found -> [] in
    transitions := HC.TMap.add lbl (t :: trans) !transitions
  in
  let iter ptr = function
  | Struct (tag, value) ->
    let () = tags.(tag) <- ptr :: tags.(tag) in
    let iter i = function
    | Int n -> push (IFieldT (i, n)) ptr ptr
    | Ptr q -> push (PFieldT i) ptr q
    | Atm t -> push (AtomT (i, t)) ptr ptr
    | Fun _ -> assert false
    in
    Array.iteri iter value
  | String s ->
    let old = try StringMap.find s !strings with Not_found -> [] in
    strings := StringMap.add s (ptr :: old) !strings
  in
  let () = Array.iteri iter mem in
  let fold _ obj accu = obj :: accu in
  let partitions = StringMap.fold fold !strings (Array.to_list tags) in
  { HC.states = size;
    transitions = !transitions;
    partitions = partitions; }

let reduce obj mem =
  if Array.length mem = 0 then (obj, mem)
  else
    let automaton = to_automaton obj mem in
    let reduced = HC.reduce_partition automaton in
    normalize obj mem reduced

let share obj mem =
  if Array.length mem = 0 then match obj with
  | Int n -> Obj.repr n
  | Atm t -> Obj.new_block t 0
  | _ -> assert false
  else
    let automaton = to_automaton obj mem in
    let reduced = HC.reduce_partition automaton in
    normalize_obj obj mem reduced

let represent obj mem =
  let init i = match mem.(i) with
  | String s -> Obj.repr (String.copy s)
  | Struct (tag, value) -> Obj.new_block tag (Array.length value)
  in
  (** Initialize the new memory with the corresponding blocks *)
  let data = Array.init (Array.length mem) init in
  let represent = function
  | Int n -> Obj.repr n
  | Ptr p -> data.(p)
  | Atm t -> Obj.new_block t 0
  | Fun _ -> assert false
  in
  let iter ptr = function
  | String _ -> ()
  | Struct (_, value) ->
    let iter i obj = Obj.set_field data.(ptr) i (represent obj) in
    Array.iteri iter value
  in
  (** Fill the inter-objects pointers *)
  let () = Array.iteri iter mem in
  (** Return the entry point *)
  represent obj
