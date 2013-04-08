(** Partition refinement algorithm *)

module type S =
sig
  type t
  type set
  val create : int -> t
  val length : t -> int
  val size : set -> t -> int
  val partition : int -> t -> set
  val iter : set -> (int -> unit) -> t -> unit
  val iter_all : (set -> unit) -> t -> unit
  val mark : int -> t -> unit
  val split : set -> t -> set
  val is_marked : set -> t -> bool
  val is_valid : set -> bool
  val choose : set -> t -> int
  val represent : set -> int
end

module Make(M : sig end) =
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

let uget (t : int array) i = Array.unsafe_get t i
let uset (t : int array) i x = Array.unsafe_set t i x

let length t = succ t.partitions

let size s t =
  uget t.last s - uget t.first s

let partition i t = uget t.index i

let iter s f t =
  let fst = uget t.first s in
  let lst = uget t.last s in
  for i = fst to lst - 1 do
    f (uget t.elements i);
  done

let iter_all f t =
  for i = 0 to t.partitions do f i; done

let next i t =
  if uget t.last (uget t.index i) < uget t.location i then -1
  else uget t.elements (succ (uget t.location i))

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
      uset nfirst i (uget pfirst i);
      uset nlast i (uget plast i);
      uset nmarked i (uget pmarked i);
    done;
    t.first <- nfirst;
    t.last <- nlast;
    t.marked <- nmarked;
  end

let split s t =
  if uget t.marked s = uget t.last s then uset t.marked s (uget t.first s);
  if uget t.marked s = uget t.first s then -1
  (** Nothing to split *)
  else begin
    let len = succ t.partitions in
    t.partitions <- len;
    resize t;
    uset t.first len (uget t.first s);
    uset t.marked len (uget t.first s);
    uset t.last len (uget t.marked s);
    uset t.first s (uget t.marked s);
    for i = uget t.first len to pred (uget t.last len) do
      uset t.index (uget t.elements i) len;
    done;
    len
  end

let mark i t =
  let set = uget t.index i in
  let loc = uget t.location i in
  let mark = uget t.marked set in
  if mark <= loc then begin
    uset t.elements loc (uget t.elements mark);
    uset t.location (uget t.elements loc) loc;
    uset t.elements mark i;
    uset t.location i mark;
    uset t.marked set (succ mark);
  end

let is_marked s t = (uget t.marked s) <> (uget t.first s)

let is_valid s = 0 <= s

let choose s t = uget t.elements (uget t.first s)

let represent s = s

end