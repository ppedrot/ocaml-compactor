open Analyze
open Compact

let main () =
  let in_file = Sys.argv.(1) in
  let out_file = in_file in
  (** Input phase *)
(*   let () = Printf.eprintf "unmarshalling...\n%!" in *)
  let in_chan = open_in in_file in
  (* magic number *)
  let magic = input_binary_int in_chan in
  (* library *)
  let (libobj, libmem) = parse_channel in_chan in
  (* digest *)
  let digest = Marshal.from_channel in_chan in
  (* table *)
  let (tableobj, tablemem) = parse_channel in_chan in
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

let () = main ()

(*let () =
  let open Dot in
  let in_file = Sys.argv.(1) in
  let in_chan = open_in in_file in
  (* magic number *)
  let _ = input_binary_int in_chan in
  (* library *)
  let (obj, mem) = parse in_chan in
  let () = close_in in_chan in
  let automaton = to_automaton obj mem in
  let reduced = HC.reduce automaton in
  let (obj, mem) = normalize obj mem reduced in
  let fold (ptr, accu) cl =
    if List.length cl < 100 then (succ ptr, accu)
    else (succ ptr, IntSet.add ptr accu)
  in
  let (_, target) = Array.fold_left fold (0, IntSet.empty) reduced in
(*   let target = Dot.back_closure mem target in *)
  Dot.pr_mem (fun ptr -> IntSet.mem ptr target) stdout (obj, mem)*)
(*   for i = 0 to pred (Array.length reduced) do *)
(*     Printf.printf "%i\n" (List.length reduced.(i)) *)
(*   done; *)

(*   let fold accu  *)
