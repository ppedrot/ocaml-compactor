open Analyze
open Compact

(** Ignore leading position information and trailing digest *)
let parse_segment chan =
  let _ = input_binary_int chan in
  let (obj, mem) = parse_channel chan in
  let digest = Digest.input chan in
  (obj, mem, digest)

let marshal_out_segment f ch v digest =
  let start = pos_out ch in
  output_binary_int ch 0;
  Marshal.to_channel ch v [];
  let stop = pos_out ch in
  seek_out ch start;
  output_binary_int ch stop;
  seek_out ch stop;
  Digest.output ch digest

let filecopy src dst =
  let buffer = String.create 1024 in
  let break = ref true in
  let out_chan = open_out dst in
  let in_chan = open_in src in
  while !break do
    let len = input in_chan buffer 0 1024 in
    let () = output out_chan buffer 0 len in
    if len = 0 then break := false
  done;
  close_in in_chan;
  close_out out_chan

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
    let (obj, mem, digest) = parse_segment in_chan in
    let obj = share obj mem in
    marshal_out_segment out_file out_chan obj digest;
    Gc.compact ()
  done;
  (* closing all *)
  close_in in_chan;
  close_out out_chan;
  (* renaming the file *)
  Sys.remove in_file;
  filecopy out_file in_file;
  Sys.remove out_file

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
