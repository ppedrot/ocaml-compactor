module type PartitionSig =
sig
  type t
  (** Type of partition structure *)
  type set
  (** Type of partitions *)
  val create : int -> t
  (** Create a partition structure of the given size *)
  val length : t -> int
  (** Number of partitions *)
  val size : set -> t -> int
  (** Number of elements of a partition *)
  val partition : int -> t -> set
  (** [partition i t] returns the index of the partition which contains [i] *)
  val iter : set -> (int -> unit) -> t -> unit
  (** Iter on elements of a partition. Don't [mark] and [split] in the loop! *)
  val iter_all : (set -> unit) -> t -> unit
  (** Iter on partitions. Don't [mark] and [split] in the loop! *)
  val mark : int -> t -> unit
  (** Mark an element for splitting *)
  val split : set -> t -> set
  (** Performs splitting and return the set of marked elements *)
  val is_marked : set -> t -> bool
  (** Returns [true] if some element of the set is marked *)
  val is_valid : set -> bool
  (** Test whether a splitting succeeded *)
  val choose : set -> t -> int
  (** Choose any element of a partition *)
  val represent : set -> int
  (** Associate a unique number to each partition *)
end

module Partition : PartitionSig =
struct

type set = int

type t = {
  mutable partitions : int;
  (** number of partitions *)
  mutable first : int array;
  (** index of the first element of a partition *)
  mutable last : int array;
  (** successor index of the last element of a partition *)
  mutable marked : int array;
  (** index of the last marked element of a partition *)
  index : set array;
  (** associate a partition to an element *)
  elements : int array;
  (** contain elements in a contiguous way w.r.t. partitions *)
  location : int array;
  (** keep the location of an element in [elements] *)
}

let initial_size = 97

let create n = {
  partitions = 0;
  first = Array.create initial_size 0;
  last = Array.create initial_size n;
  marked = Array.create initial_size 0;
  index = Array.create n 0;
  elements = Array.init n (fun i -> i);
  location = Array.init n (fun i -> i);
}

let length t = succ t.partitions

let size s t = t.last.(s) - t.first.(s)

let partition i t = t.index.(i)

let iter s f t =
  let i = ref t.first.(s) in
  while !i < t.last.(s) do
    f t.elements.(!i);
    incr i
  done

let iter_all f t =
  for i = 0 to t.partitions do f i; done

let next i t =
  if t.last.(t.index.(i)) < t.location.(i) then -1
  else t.elements.(succ t.location.(i))

let resize t =
  let len = Array.length t.first in
  if len <= t.partitions then begin
    let nlen = 2 * len + 1 in
    let pfirst = t.first in
    let plast = t.last in
    let pmarked = t.marked in
    let nfirst = Array.make nlen 0 in
    let nlast = Array.make nlen 0 in
    let nmarked = Array.make nlen 0 in
    for i = 0 to pred len do
      nfirst.(i) <- pfirst.(i);
      nlast.(i) <- plast.(i);
      nmarked.(i) <- pmarked.(i);
    done;
    t.first <- nfirst;
    t.last <- nlast;
    t.marked <- nmarked;
  end

let split s t =
  if t.marked.(s) = t.last.(s) then t.marked.(s) <- t.first.(s);
  if t.marked.(s) = t.first.(s) then -1
  else begin
    let len = succ t.partitions in
    t.partitions <- len;
    resize t;
    t.first.(len) <- t.first.(s);
    t.marked.(len) <- t.first.(s);
    t.last.(len) <- t.marked.(s);
    t.first.(s) <- t.marked.(s);
    for i = t.first.(len) to pred t.last.(len) do
      t.index.(t.elements.(i)) <- len;
    done;
    len
  end

let mark i t =
  let set = t.index.(i) in
  let loc = t.location.(i) in
  let mark = t.marked.(set) in
  if mark <= loc then begin
    t.elements.(loc) <- t.elements.(mark);
    t.location.(t.elements.(loc)) <- loc;
    t.elements.(mark) <- i;
    t.location.(i) <- mark;
    t.marked.(set) <- succ mark;
  end

let is_marked s t = t.marked.(s) <> t.first.(s)

let is_valid s = 0 <= s

let choose s t = t.elements.(t.first.(s))

let represent s = s

end

type ('label, 'state) transition = {
  lbl : 'label;
  src : 'state;
  dst : 'state;
}
type ('label, 'state) automaton = {
  states : int;
  (** The number of states of the automaton *)
  final_states : 'state array;
  (** The final states *)
  transitions : ('label, 'state) transition array;
  (** The transitions of the automaton without duplicates *)
}

module type OrderedType =
sig
  type t
  val compare : t -> t -> int
end

module type S =
sig
  type label
  type state = int
  val reduce : (label, state) automaton -> state list array
  (** Associate the array of equivalence classes of the states of an automaton *)
end

module Make (Label : OrderedType) : S with type label = Label.t =
struct

type label = Label.t
type state = int

(** Partitions of states *)
module SPartition : PartitionSig = Partition

(** Partitions of transitions *)
module TPartition : PartitionSig = Partition

type environment = {
  state_partition : SPartition.t;
  state_touched : SPartition.set Stack.t;
  splitter_partition : TPartition.t;
  splitter_todo : TPartition.set Stack.t;
  splitter_touched : TPartition.set Stack.t;
}

(** Associate the list of transitions ending in a given state *)
let reverse automaton =
  let ans = Array.make automaton.states [] in
  let add (x : int) l = (* if List.mem x l then l else *) x :: l in
  let iter i trans =
    let l = ans.(trans.dst) in
    ans.(trans.dst) <- add i l
  in
  let () = Array.iteri iter automaton.transitions in
  ans

let init automaton =
  let compare t1 t2 =
    let ans = Label.compare t1.lbl t2.lbl in
    if ans = 0 then
      let ans = Pervasives.compare t1.src t2.src in
      if ans = 0 then Pervasives.compare t1.dst t2.dst
      else ans
    else ans
  in
  let () = Array.sort compare automaton.transitions in
  let env = {
    state_partition = SPartition.create automaton.states;
    state_touched = Stack.create ();
    splitter_partition = TPartition.create (Array.length automaton.transitions);
    splitter_todo = Stack.create ();
    splitter_touched = Stack.create ();
  } in
  (** Split splitters according to their label *)
  let len = Array.length automaton.transitions in
  if len > 0 then begin
    let p = env.splitter_partition in
    let label = ref automaton.transitions.(0).lbl in
    (** pt is initial, full partition *)
    let pt = TPartition.partition 0 p in
    let i = ref 0 in
    while !i < len do
      (** Each time the label changes, we split *)
      let nlbl = automaton.transitions.(!i).lbl in
      if !label <> nlbl then begin
        ignore (TPartition.split pt p);
        label := nlbl
      end;
      TPartition.mark !i p;
      incr i
    done;
    ignore (TPartition.split pt p);
  end;
  (** Push every splitter in the todo stack *)
  let iter pt = Stack.push pt env.splitter_todo in
  TPartition.iter_all iter env.splitter_partition;
  (** Mark every final state and split *)
  let ps = SPartition.partition 0 env.state_partition in
  let iter state = SPartition.mark state env.state_partition in
  Array.iter iter automaton.final_states;
  ignore (SPartition.split ps env.state_partition);
  env

let split_partition s inv env =
  let p = env.state_partition in
  let r = SPartition.split s p in
  if SPartition.is_valid r then begin
    let r = if SPartition.size r p < SPartition.size s p then r else s in
    let iter state =
      let iter trans =
        let pt = TPartition.partition trans env.splitter_partition in
        if not (TPartition.is_marked pt env.splitter_partition) then
          Stack.push pt env.splitter_touched;
        TPartition.mark trans env.splitter_partition
      in
      List.iter iter inv.(state)
    in
    SPartition.iter r iter p;
    while not (Stack.is_empty env.splitter_touched) do
      let pt = Stack.pop env.splitter_touched in
      let npt = TPartition.split pt env.splitter_partition in
      if TPartition.is_valid npt then Stack.push npt env.splitter_todo
    done
  end

let reduce_aux automaton =
  let env = init automaton in
  let inv = reverse automaton in
  while not (Stack.is_empty env.splitter_todo) do
    let pt = Stack.pop env.splitter_todo in
    let iter t =
      let previous = automaton.transitions.(t).src in
      let equiv = SPartition.partition previous env.state_partition in
      if not (SPartition.is_marked equiv env.state_partition) then
        Stack.push equiv env.state_touched;
      SPartition.mark previous env.state_partition
    in
    TPartition.iter pt iter env.splitter_partition;
    while not (Stack.is_empty env.state_touched) do
      let equiv = Stack.pop env.state_touched in
      split_partition equiv inv env
    done
  done;
  (env, inv)

let reduce automaton =
  let (ans, _) = reduce_aux automaton in
  let mapping = Array.create (SPartition.length ans.state_partition) [] in
  let iter set =
    let pi = SPartition.represent set in
    let iter i = mapping.(pi) <- i :: mapping.(pi) in
    SPartition.iter set iter ans.state_partition
  in
  let () = SPartition.iter_all iter ans.state_partition in
  mapping

end
(*     Printf.printf "%i ~ { " (SPartition.represent set);
    SPartition.iter set (Printf.printf "%i ") ans.state_partition;
    Printf.printf "}\n%!";
 *)
(* let reduce automaton =
  let ans, inv = reduce_aux automaton in
  let size = ref 0 in
  let final_states = ref [] in
  let transitions = ref [] in
  let mem x a =
    let len = Array.length a in
    let i = ref 0 in
    let break = ref true in
    while !break && !i < len do
      if a.(!i) = x then break := false else incr i;
    done;
    not !break
  in
  let iter p =
    let () = incr size in
    let i = SPartition.choose p ans.state_partition in
    if mem i automaton.final_states then final_states := i :: !final_states;
    let pred = inv.(i) in
    let iter ti =
      let t = automaton.transitions.(ti) in
      let srcq = SPartition.partition t.src ans.state_partition in
      let dstq = SPartition.partition t.dst ans.state_partition in
      let tq = {
        lbl = t.lbl;
        src = SPartition.represent srcq;
        dst = SPartition.represent dstq;
      } in
      transitions := tq :: !transitions
    in
    List.iter iter pred
  in
  let () = SPartition.iter_all iter ans.state_partition in
  let pinitial = SPartition.partition automaton.initial_state ans.state_partition in
  {
    states = !size;
    initial_state = SPartition.represent pinitial;
    final_states = Array.of_list !final_states;
    transitions = Array.of_list !transitions;
  }

let tr s d l = {src = s; dst = d; lbl = l}

let automaton = {
  states = 3;
  initial_state = 0;
  final_states = [||];
  transitions = [|tr 0 1 42; tr 1 0 42; tr 2 0 18|]
}*)

(*let print p =
  let iter set =
    Printf.printf "{ ";
    Partition.iter set (Printf.printf "%i ") p;
    Printf.printf "}\n%!";
  in
  Partition.iter_all iter p*)
(*   let ans = reduce automaton in *)
(*   let pr_tr t = Printf.printf "%i --> %i :: %i\n%!" t.src t.dst t.lbl in *)
(*   Printf.printf "size:%i initial:%i\n%!" ans.states ans.initial_state; *)
(*   Array.iter pr_tr ans.transitions *)
