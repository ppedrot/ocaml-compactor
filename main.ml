open Analyze
open Compact

(** Ignore leading position information and trailing digest *)
let parse_channel chan =
  let _ = input_binary_int chan in
  let data = parse_channel chan in
  let _ = Digest.input chan in
  data

let marshal_out_segment f ch v =
  let start = pos_out ch in
  output_binary_int ch 0;
  Marshal.to_channel ch v [];
  let stop = pos_out ch in
  seek_out ch start;
  output_binary_int ch stop;
  seek_out ch stop;
  Digest.output ch (Digest.file f)

let main () =
  let in_file = Sys.argv.(1) in
  (** Input phase *)
(*   let () = Printf.eprintf "unmarshalling...\n%!" in *)
  let in_chan = open_in in_file in
  let (out_file, out_chan) = Filename.open_temp_file "compact" "" in
  (* magic number *)
  let magic = input_binary_int in_chan in
  let () = output_binary_int out_chan magic in
  (* segments; there are 5 of them in 8.5 *)
  for i = 0 to 4 do
(*   let () = Printf.eprintf "library: %i objects\n%!" (Array.length libmem) in *)
(*   let () = Printf.eprintf "compact library: %i objects\n%!" (Array.length libmem) in *)
    let (obj, mem) = parse_channel in_chan in
    let (obj, mem) = reduce obj mem in
    let obj = represent obj mem in
    marshal_out_segment out_file out_chan obj
  done;
  (* closing all *)
  close_in in_chan;
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
