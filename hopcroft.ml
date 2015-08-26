module type OrderedType =
sig
  type t
  val compare : t -> t -> int
end

module type S =
sig
  type label
  type state
  type transition = {
    lbl : label;
    src : state;
    dst : state;
  }

  module TSet : Set.S with type elt = transition

  type automaton = {
    states : int;
    (** The number of states of the automaton *)
    partitions : state list array;
    (** A set of state partitions *)
    transitions : TSet.t;
    (** The transitions of the automaton *)
  }

  val reduce : automaton -> state list array
  (** Associate the array of equivalence classes of the states of an automaton *)

  module SPartition : Partition.S

  val reduce_partition : automaton -> SPartition.t
end

module Make (Label : OrderedType) : S
  with type label = Label.t
  and type state = int =
struct

type label = Label.t
type state = int

type transition = {
  lbl : label;
  src : state;
  dst : state;
}

module TransitionOrd =
struct
  type t = transition

  let int_compare (x : int) y = Pervasives.compare x y

  let compare t1 t2 =
    let c = Label.compare t1.lbl t2.lbl in
    if c <> 0 then c
    else
      let c = int_compare t1.src t2.src in
      if c <> 0 then c else int_compare t1.dst t2.dst
end

module TSet =
struct
  include Set.Make(TransitionOrd)

  let iteri f s =
    let f t i = f i t; succ i in
    ignore (fold f s 0)

end

type automaton = {
  states : int;
  partitions : state list array;
  transitions : TSet.t;
}

(** Partitions of states *)
module SPartition : Partition.S = Partition

(** Partitions of transitions *)
module TPartition : Partition.S = Partition

type environment = {
  state_partition : SPartition.t;
  splitter_partition : TPartition.t;
  transition_source : int array;
}

(** Associate the list of transitions ending in a given state *)
let reverse automaton =
  let ans = Array.make automaton.states [] in
  let add (x : int) l = (* if List.mem x l then l else *) x :: l in
  let iter i trans =
    let l = Array.unsafe_get ans trans.dst in
    Array.unsafe_set ans trans.dst (add i l)
  in
  let () = TSet.iteri iter automaton.transitions in
  ans

let init automaton =
  let transitions = automaton.transitions in
  let len = TSet.cardinal transitions in
  (** Sort transitions according to their label *)
  let env = {
    state_partition = SPartition.create automaton.states;
    splitter_partition = TPartition.create len;
    transition_source = Array.create len (-1);
  } in
  (** Set the source of the transitions *)
  let iteri i trans = env.transition_source.(i) <- trans.src in
  TSet.iteri iteri transitions;
  (** Split splitters according to their label *)
  if len > 0 then begin
    let p = env.splitter_partition in
    let label = ref (TSet.min_elt transitions).lbl in
    (** pt is initial, full partition *)
    let pt = TPartition.partition 0 p in
    let iteri i trans =
      (** Each time the label changes, we split *)
      let nlbl = trans.lbl in
      if Label.compare !label nlbl <> 0 then begin
        ignore (TPartition.split pt p);
        label := nlbl
      end;
      TPartition.mark i p
    in
    TSet.iteri iteri transitions;
    ignore (TPartition.split pt p);
  end;
  (** Push every splitter in the todo stack *)
  let fold pt todo = pt :: todo in
  let splitter_todo = TPartition.fold_all fold env.splitter_partition [] in
  (** Mark every state in each partition and split *)
  let separate pt =
    let ps = SPartition.partition 0 env.state_partition in
    let iter state = SPartition.mark state env.state_partition in
    List.iter iter pt;
    ignore (SPartition.split ps env.state_partition)
  in
  Array.iter separate automaton.partitions;
  env, splitter_todo

let split_partition s inv env todo =
  let p = env.state_partition in
  let r = SPartition.split s p in
  if SPartition.is_valid r then begin
    let r = if SPartition.size r p < SPartition.size s p then r else s in
    let fold state accu =
      let fold accu trans =
        let pt = TPartition.partition trans env.splitter_partition in
        let accu =
          if TPartition.is_marked pt env.splitter_partition then accu
          else pt :: accu
        in
        let () = TPartition.mark trans env.splitter_partition in
        accu
      in
      List.fold_left fold accu inv.(state)
    in
    let splitter_touched = SPartition.fold r fold p [] in
    let fold_touched todo pt =
      let npt = TPartition.split pt env.splitter_partition in
      if TPartition.is_valid npt then npt :: todo
      else todo
    in
    List.fold_left fold_touched todo splitter_touched
  end else
    todo

let reduce_aux automaton =
  let env, splitter_todo = init automaton in
  let inv = reverse automaton in
  let rec loop = function
  | [] -> ()
  | pt :: todo ->
    let fold t state_touched =
      let previous = env.transition_source.(t) in
      let equiv = SPartition.partition previous env.state_partition in
      let state_touched =
        if SPartition.is_marked equiv env.state_partition then state_touched
        else equiv :: state_touched
      in
      let () = SPartition.mark previous env.state_partition in
      state_touched
    in
    let state_touched = TPartition.fold pt fold env.splitter_partition [] in
    let fold_touched todo equiv = split_partition equiv inv env todo in
    let splitter_todo = List.fold_left fold_touched todo state_touched in
    loop splitter_todo
  in
  let () = loop splitter_todo in
  (env, inv)

let reduce automaton =
  let (ans, _) = reduce_aux automaton in
  let mapping = Array.create (SPartition.length ans.state_partition) [] in
  let iter set =
    let pi = SPartition.represent set in
    let iter i =
      let map = Array.unsafe_get mapping pi in
      Array.unsafe_set mapping pi (i :: map)
    in
    SPartition.iter set iter ans.state_partition
  in
  let () = SPartition.iter_all iter ans.state_partition in
  mapping

  let reduce_partition automaton =
    let (ans, _) = reduce_aux automaton in
    ans.state_partition

end
