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

let reduce obj mem =
  (** Create the automaton *)
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

let reduce obj mem =
  if Array.length mem = 0 then (obj, mem) else reduce obj mem

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

let () =
  let in_file = Sys.argv.(1) ^ "o" in
  let out_file = in_file in
  (** Input phase *)
(*   let () = Printf.eprintf "unmarshalling...\n%!" in *)
  let in_chan = open_in in_file in
  (* magic number *)
  let magic = input_binary_int in_chan in
  (* library *)
  let (libobj, libmem) = parse in_chan in
  (* digest *)
  let digest = Marshal.from_channel in_chan in
  (* table *)
  let (tableobj, tablemem) = parse in_chan in
  let () = close_in in_chan in
  (** Reduce phase *)
  let (libobj, libmem) = reduce libobj libmem in
  let libobj = represent libobj libmem in
  let (tableobj, tablemem) = reduce tableobj tablemem in
  let tableobj = represent tableobj tablemem in
  (** Output phase *)
  let out_chan = open_out out_file in
  (** magic number *)
  let () = output_binary_int out_chan magic in
  (** library *)
(*   let () = Printf.eprintf "library: %i objects\n%!" (Array.length libmem) in *)
(*   let () = Printf.eprintf "compact library: %i objects\n%!" (Array.length libmem) in *)
  let () = Marshal.to_channel out_chan libobj [] in
  (** digest *)
  let () = Marshal.to_channel out_chan digest [] in
  (** table *)
(*   let () = Printf.eprintf "table: %i objects\n%!" (Array.length tablemem) in *)
(*   let () = Printf.eprintf "compact table: %i objects\n%!" (Array.length tablemem) in *)
  let () = Marshal.to_channel out_chan tableobj [] in
  (* closing all *)
  close_out out_chan
