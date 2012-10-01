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
  let compare = Pervasives.compare
end

module HC = Hopcroft.Make(TransitionOrd)

let normalize obj mem cl =
  let size = Array.length mem in
  let assoc = Array.make size (-1) in
  let compact_mem = Array.make (Array.length cl) (Struct (-1, [||])) in
  let iter cli elts = List.iter (fun i -> assoc.(i) <- cli) elts in
  let () = Array.iteri iter cl in
  let canonical content = match content with
  | Int _ | Atm _ -> content
  | Ptr p -> Ptr assoc.(p)
  in
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
  (canonical obj, compact_mem)

let reduce obj mem =
  let size = Array.length mem in
  let transitions = ref [] in
  let push lbl src dst =
    let t = { Hopcroft.lbl = lbl; src = src; dst = dst } in
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
  let automaton = {
    Hopcroft.states = size;
    transitions = Array.of_list !transitions;
    final_states = [||];
  } in
  let reduced = HC.reduce automaton in
  normalize obj mem reduced

let () =
  let file = Sys.argv.(1) in
  let () = Printf.eprintf "unmarshalling...\n%!" in
  let chan = open_in file in
  let _ = input_binary_int chan in
  let (obj, mem) = parse chan in
  let () = Printf.eprintf "library: %i objects\n%!" (Array.length mem) in
  let (_, mem) = reduce obj mem in
  let () = Printf.eprintf "compact library: %i objects\n%!" (Array.length mem) in
  let _ = parse chan in
  let (obj, mem) = parse chan in
  let _ = reduce obj mem in
  close_in chan
