open Analyze

type transition =
| StringT of string
| TagT of int
| AtomT of int * int (* field number * tag *)
| PFieldT of int (* field number *)
| IFieldT of int * int (* field number * int value *)

module TransitionOrd =
struct
  type t = transition
  let int_compare (x : int) y = Pervasives.compare x y
  let str_compare (x : string) y = Pervasives.compare x y

  let compare t1 t2 = match t1, t2 with
  | StringT s1, StringT s2 -> str_compare s1 s2
  | TagT t1, TagT t2 -> int_compare t1 t2
  | AtomT (n1, t1), AtomT (n2, t2) ->
    let c = int_compare n1 n2 in
    if c = 0 then int_compare t1 t2 else c
  | PFieldT n1, PFieldT n2 -> int_compare n1 n2
  | IFieldT (n1, i1), IFieldT (n2, i2) ->
    let c = int_compare n1 n2 in
    if c = 0 then int_compare i1 i2 else c

  | StringT _, (TagT _ | AtomT (_, _) | PFieldT _ | IFieldT (_, _)) -> -1
  | TagT _, (AtomT (_, _) | PFieldT _ | IFieldT (_, _)) -> -1
  | AtomT (_, _), (PFieldT _ | IFieldT (_, _)) -> -1
  | PFieldT _, (IFieldT (_, _)) -> -1

  | TagT _, (StringT _) -> 1
  | AtomT (_, _), (StringT _ | TagT _) -> 1
  | PFieldT _, (StringT _ | TagT _ | AtomT (_, _)) -> 1
  | IFieldT (_, _), (StringT _ | TagT _ | AtomT (_, _) | PFieldT _) -> 1

end

module HC = Hopcroft.Make(TransitionOrd)

let normalize obj mem cl =
  let size = Array.length mem in
  (** [assoc.(i)] contains the equivalence class of [i]. *)
  let assoc = Array.make size (-1) in
  let iter cli elts = List.iter (fun i -> assoc.(i) <- cli) elts in
  let () = Array.iteri iter cl in
  (** Initialize the new memory with dummy values *)
  let compact_mem = Array.make (Array.length cl) (Struct (-1, [||])) in
  let canonical content = match content with
  | Int _ | Atm _ -> content
  | Ptr p -> Ptr assoc.(p)
  in
  (** Fill the new memory with canonical names *)
  let iter idx cli =
    (** Choose an element *)
    let repr = List.hd cli in
    let data = match mem.(repr) with
    | Struct (tag, value) -> Struct (tag, Array.map canonical value)
    | String s -> String s
    in
    compact_mem.(idx) <- data
  in
  let () = Array.iteri iter cl in
  (** Return canonical entry point and compacted memory *)
  (canonical obj, compact_mem)

let to_automaton obj mem =
  (** Create the automaton *)
  let size = Array.length mem in
  let transitions = ref [] in
  let push lbl src dst =
    let t = { HC.lbl = lbl; src = src; dst = dst } in
    transitions := t :: !transitions
  in
  let iter ptr = function
  | Struct (tag, value) ->
    let () = push (TagT tag) ptr ptr in
    let iter i = function
    | Int n -> push (IFieldT (i, n)) ptr ptr
    | Ptr q -> push (PFieldT i) ptr q
    | Atm t -> push (AtomT (i, t)) ptr ptr
    in
    Array.iteri iter value
  | String s ->
    push (StringT s) ptr ptr
  in
  let () = Array.iteri iter mem in
  { HC.states = size;
    transitions = Array.of_list !transitions;
    final_states = [||]; }

let reduce obj mem =
  if Array.length mem = 0 then (obj, mem)
  else
    let automaton = to_automaton obj mem in
    let reduced = HC.reduce automaton in
    normalize obj mem reduced

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
