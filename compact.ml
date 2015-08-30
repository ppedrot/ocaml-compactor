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

let int_stack x =
  let ans = new Container.int_stack in
  ans#push x;
  ans

let trans_stack x =
  let ans = new Container.transition_stack in
  ans#push x;
  ans

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
    let t = { Hopcroft.src = src; dst = dst } in
    (try (HC.TMap.find lbl !transitions)#push t
    with Not_found -> transitions := HC.TMap.add lbl (trans_stack t) !transitions)
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
    (try (FieldsMap.find key !fields)#push ptr
    with Not_found -> fields := FieldsMap.add key (int_stack ptr) !fields)
  | String s ->
    (try (StringMap.find s !strings)#push ptr
    with Not_found -> strings := StringMap.add s (int_stack ptr) !strings)
  in
  let () = Array.iteri iter mem in
  let fold _ obj accu = (obj :> _ Hopcroft.foldable) :: accu in
  let partitions = FieldsMap.fold fold !fields (StringMap.fold fold !strings []) in
  { HC.states = size;
    transitions = (!transitions :> _ Hopcroft.foldable HC.TMap.t);
    partitions = partitions; }

type context = {
  mutable obj_index : int;
  obj_total : int;
  mutable fields : int Container.stack FieldsMap.t;
  mutable strings : int Container.stack StringMap.t;
  mutable obj_stack : int list;
  mutable off_stack : int list;
  mutable len_stack : int list;
  mutable fld_stack : Fields.t list;
  mutable transitions : int Hopcroft.transition Container.stack HC.TMap.t;
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
      (try (FieldsMap.find fld ctx.fields)#push obj
      with Not_found -> ctx.fields <- FieldsMap.add fld (int_stack obj) ctx.fields);
      cleanup ctx
    end
  | _ -> assert false

let listener =
  let iheader h = {
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
    let t = { Hopcroft.src = src; dst = dst } in
    (try (HC.TMap.find lbl ctx.transitions)#push t
    with Not_found -> ctx.transitions <- HC.TMap.add lbl (trans_stack t) ctx.transitions)
  in
  let ievent ev ctx =
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
      (try (StringMap.find s ctx.strings)#push ctx.obj_index
      with Not_found -> ctx.strings <- StringMap.add s (int_stack ctx.obj_index) ctx.strings);
      ctx.obj_index <- ctx.obj_index + 1;
      cleanup ctx;
    in

    ctx
  in
  let iclose ctx =
    assert (ctx.obj_stack = []);
    assert (ctx.obj_index = ctx.obj_total);
    ctx
  in
  { iheader; ievent; iclose }

let to_automaton_async chan =
  let ctx = listen_channel chan listener in
  let fold _ obj accu = (obj :> _ Hopcroft.foldable) :: accu in
  let partitions = FieldsMap.fold fold ctx.fields [] in
  let partitions = StringMap.fold fold ctx.strings partitions in
  { HC.states = ctx.obj_total;
    transitions = (ctx.transitions :> _ Hopcroft.foldable HC.TMap.t);
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

type tcontext = {
  mutable tobj_index : int;
  (** Offset of the current object in the intput structure *)
  mutable tobj_output : int;
  (** Offset of the current object in the output structure *)
  tobj_total : int;
  (** Number of expected objects *)
  mutable tskp_stack : bool list;
  mutable toff_stack : int list;
}

let canonicalize size part =
  let ans = Array.make size (-1) in
  let iter s =
    let get_min (i : int) accu = if i < accu then i else accu in
    let min = HC.SPartition.fold s get_min part max_int in
    HC.SPartition.iter s (fun i -> ans.(i) <- min) part
  in
  let () = HC.SPartition.iter_all iter part in
  ans

let rec tcleanup ctx =
  match ctx.tskp_stack, ctx.toff_stack with
  | [], [] -> ()
  | _ :: skps, off :: offs ->
    if off = 0 then begin
      ctx.tskp_stack <- skps;
      ctx.toff_stack <- offs;
      tcleanup ctx
    end
  | _ -> assert false

let transduce ichan ochan =
  let init_pos = pos_in ichan in
  let automaton = to_automaton_async ichan in
  let states = automaton.HC.states in
  let partition = HC.reduce_partition automaton in
  let reduced = HC.SPartition.length partition in
  let canonical = canonicalize states partition in
  let is_canonical ptr = canonical.(ptr) < 0 || ptr = canonical.(ptr) in
  let rec get_canonical ptr =
    let repr = canonical.(ptr) in
    if repr < 0 then - repr else get_canonical repr
  in
  let echoer = echo_channel ochan in
  let iheader h = {
    tobj_index = 0;
    tobj_output = 0;
    tobj_total = h.objects;
    tskp_stack = [];
    toff_stack = [];
  } in
  let ievent ev ctx =
    let keep = match ctx.tskp_stack with [] -> true | can :: _ -> can in
    if keep then begin match ev with
    | RInt _ | RBlock (_, 0) -> echoer.oevent ev
    | RPointer off ->
      let ptr = get_canonical (ctx.tobj_index - off) in
      echoer.oevent (RPointer ptr)
    | RBlock (_, _) | RString _ ->
      let can = is_canonical ctx.tobj_index in
      if can then begin
        echoer.oevent ev;
        canonical.(ctx.tobj_index) <- (- ctx.tobj_output);
        ctx.tobj_output <- 1 + ctx.tobj_output;
      end else
        echoer.oevent (RPointer (get_canonical ctx.tobj_index));
    | RCode _ -> assert false
    end;
    begin match ctx.toff_stack with
    | [] -> ()
    | off :: offs -> ctx.toff_stack <- pred off :: offs;
    end;
    begin match ev with
    | RInt _ | RBlock (_, 0) | RPointer _ -> tcleanup ctx
    | RBlock (_, len) ->
      let can = is_canonical ctx.tobj_index in
      ctx.tskp_stack <- can :: ctx.tskp_stack;
      ctx.toff_stack <- len :: ctx.toff_stack;
      ctx.tobj_index <- ctx.tobj_index + 1;
    | RString _ ->
      ctx.tobj_index <- ctx.tobj_index + 1;
      tcleanup ctx
    | RCode _ -> assert false
    end;
    ctx
  in
  let iclose ctx =
    let header = echoer.oclose () in
    assert (ctx.tskp_stack = []);
    assert (ctx.tobj_index = states);
    assert (header.objects = reduced);
    ctx
  in
  let listener = { iheader; ievent; iclose; } in
  (** Go back and output the reduced structure online *)
  let () = seek_in ichan init_pos in
  ignore (listen_channel ichan listener);
