open Analyze

module Fields =
struct
  type t = int list
    (** Lists of the form [t; f1; v1; ...; fn; vn where
        - t is the tag of the objects
        - fi is the position of the field
        - vi is the value of the field
        and the list [fi, vi] is ordered according to fi
    *)

  let int_compare (x : int) y = Pervasives.compare x y

  let rec compare l1 l2 = match l1, l2 with
  | [], [] -> 0
  | [], _ :: _ -> 1
  | _ :: _, [] -> -1
  | xi :: li, xj :: lj ->
    let c = int_compare xi xj in
    if c <> 0 then c else compare li lj

end

module FieldsMap = Map.Make(Fields)

type transition =
| AtomT of int * int (* field number * tag *)
| PFieldT of int (* field number *)

module TransitionOrd =
struct
  type t = transition

  let int_compare (x : int) y = Pervasives.compare x y

  let compare t1 t2 = match t1, t2 with
  | AtomT (n1, t1), AtomT (n2, t2) ->
    let c = int_compare n1 n2 in
    if c = 0 then int_compare t1 t2 else c
  | PFieldT n1, PFieldT n2 -> int_compare n1 n2
  | AtomT (_, _), PFieldT _ -> -1

  | PFieldT _, (AtomT (_, _)) -> 1

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
  let fields = ref FieldsMap.empty in
  let strings = ref StringMap.empty in
  let push lbl src dst =
    let t = { HC.src = src; dst = dst } in
    let trans = try HC.TMap.find lbl !transitions with Not_found -> [] in
    transitions := HC.TMap.add lbl (t :: trans) !transitions
  in
  let iter ptr = function
  | Struct (tag, value) ->
    let fold (i, accu) = function
    | Int n -> (succ i, i :: n :: accu)
    | Ptr q -> push (PFieldT i) ptr q; (succ i, accu)
    | Atm t -> push (AtomT (i, t)) ptr ptr; (succ i, accu)
    | Fun _ -> assert false
    in
    let (_, fs) = Array.fold_left fold (0, []) value in
    let key = tag :: fs in
    let old = try FieldsMap.find key !fields with Not_found -> [] in
    fields := FieldsMap.add key (ptr :: old) !fields
  | String s ->
    let old = try StringMap.find s !strings with Not_found -> [] in
    strings := StringMap.add s (ptr :: old) !strings
  in
  let () = Array.iteri iter mem in
  let fold _ obj accu = obj :: accu in
  let partitions = FieldsMap.fold fold !fields (StringMap.fold fold !strings []) in
  { HC.states = size;
    transitions = !transitions;
    partitions = partitions; }

type context = {
  mutable obj_index : int;
  obj_total : int;
  mutable fields : int list FieldsMap.t;
  mutable strings : int list StringMap.t;
  mutable obj_stack : int list;
  mutable off_stack : int list;
  mutable len_stack : int list;
  mutable fld_stack : Fields.t list;
  mutable transitions : HC.transition list HC.TMap.t;
}

let rec cleanup ctx =
  match ctx.obj_stack, ctx.off_stack, ctx.len_stack, ctx.fld_stack with
  | [], [], [], [] -> ()
  | obj :: objs, off :: offs, len :: lens, fld :: flds ->
    if off = len then begin
      ctx.obj_stack <- objs;
      ctx.off_stack <- offs;
      ctx.len_stack <- lens;
      ctx.fld_stack <- flds;
      let old = try FieldsMap.find fld ctx.fields with Not_found -> [] in
      ctx.fields <- FieldsMap.add fld (obj :: old) ctx.fields;
      cleanup ctx
    end
  | _ -> assert false

let listener =
  let header h = {
    obj_index = 0;
    obj_total = h.objects;
    fields = FieldsMap.empty;
    strings = StringMap.empty;
    obj_stack = [];
    off_stack = [];
    len_stack = [];
    fld_stack = [];
    transitions = HC.TMap.empty;
  } in
  let push ctx lbl src dst =
    let t = { HC.src = src; dst = dst } in
    let trans = try HC.TMap.find lbl ctx.transitions with Not_found -> [] in
    ctx.transitions <- HC.TMap.add lbl (t :: trans) ctx.transitions
  in
  let event ev ctx =
    let () = match ctx.obj_stack, ctx.off_stack, ctx.fld_stack with
    | [], [], [] -> ()
    | ptr :: _, i :: offs, f :: flds ->
      begin match ev with
      | RInt n -> ctx.fld_stack <- (i :: n :: f) :: flds
      | RBlock (tag, 0) -> push ctx (AtomT (i, tag)) ptr ptr
      | RBlock _ | RString _ -> push ctx (PFieldT i) ptr ctx.obj_index
      | RPointer off -> push ctx (PFieldT i) ptr (ctx.obj_index - off)
      | RCode _ -> assert false
      end;
      ctx.off_stack <- (succ i) :: offs;
    | _ -> assert false
    in
    let () = match ev with
    | RInt _ | RBlock (_, 0) | RPointer _ | RCode _ -> cleanup ctx
    | RBlock (tag, len) ->
      ctx.obj_stack <- ctx.obj_index :: ctx.obj_stack;
      ctx.off_stack <- 0 :: ctx.off_stack;
      ctx.len_stack <- len :: ctx.len_stack;
      ctx.fld_stack <- [tag] :: ctx.fld_stack;
      ctx.obj_index <- ctx.obj_index + 1;
    | RString s ->
      let old = try StringMap.find s ctx.strings with Not_found -> [] in
      ctx.strings <- StringMap.add s (ctx.obj_index :: old) ctx.strings;
      ctx.obj_index <- ctx.obj_index + 1;
      cleanup ctx;
    in

    ctx
  in
  let close ctx =
    assert (ctx.obj_stack = []);
    assert (ctx.obj_index = ctx.obj_total);
    ctx
  in
  { header; event; close }

let to_automaton_async chan =
  let ctx = listen_channel chan listener in
  let fold _ obj accu = obj :: accu in
  let partitions = FieldsMap.fold fold ctx.fields [] in
  let partitions = StringMap.fold fold ctx.strings partitions in
  { HC.states = ctx.obj_total;
    transitions = ctx.transitions;
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
