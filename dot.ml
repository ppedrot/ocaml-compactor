open Analyze

module IntOrd =
struct
  type t = int
  let compare = compare
end

module IntMap = Map.Make(IntOrd)
module IntSet = Set.Make(IntOrd)

let pr_data chan = function
| Int i -> Printf.fprintf chan "i:%i" i
| Ptr ptr -> Printf.fprintf chan "p:0x%08x" ptr
| Atm tag -> Printf.fprintf chan "atom:0x%02x" tag

let pr_obj chan = function
| Struct (i, [|x|]) ->
  Printf.fprintf chan "%i[%a]" i pr_data x
| Struct (i, v) ->
  let len = Array.length v in
  let rec pr_list chan i =
    if len <= i then ()
    else if pred len = i then pr_data chan v.(i)
    else
      Printf.fprintf chan "%a %a" pr_data v.(i) pr_list (succ i)
  in
  Printf.fprintf chan "%i[%a]" i pr_list 0
| String s -> Printf.fprintf chan "\"%s\"" (String.escaped s)

let pr_mem chan obj mem =
  let iter id obj = Printf.fprintf chan "0x%08x -> %a\n" id pr_obj obj in
  Array.iteri iter mem

let pr_atom chan = function
| Struct (_, value) ->
  let iter = function
  | Atm tag ->
    Printf.fprintf chan "atom_%02x[label=\"atom::%02x\", fillcolor=\"red\", style=\"filled\"];\n" tag tag
  | _ -> ()
  in
  Array.iter iter value
| String _ -> ()

let pr_dot_obj ptr chan = function
| Struct (i, v) ->
  let pr_field i c =
  let cp = match c with
  | Int i -> string_of_int i
  | Atm _ | Ptr _ -> "_"
  in
    Printf.sprintf "<f%i>%s" i cp
  in
  let fields = Array.mapi pr_field v in
  let fields = String.concat "|" (Array.to_list fields) in
  Printf.fprintf chan "[label=\"struct:%08x::%i|%s\"]" ptr i fields
| String s ->
  Printf.fprintf chan "[label=\"string:%08x\", fillcolor=\"green\", style=\"filled\"]" ptr

let pr_dot_link test ptr chan = function
| Struct (i, [||]) -> ()
| Struct (i, v) ->
  let len = Array.length v in
  for i = 0 to len - 1 do
    match v.(i) with
    | Ptr dst ->
      if test dst then
        Printf.fprintf chan "struct_%08x:f%i -> struct_%08x;\n" ptr i dst
    | Atm tag ->
      Printf.fprintf chan "struct_%08x:f%i -> atom_%02x;\n" ptr i tag
    | Int _ -> ()
  done
| _ -> ()

let closure mem target =
  (* compute reverse pointers *)
  let rev_ptr = Array.make (Array.length mem) [] in
  let iter ptr = function
  | Struct (i, v) ->
    let iter = function
    | Ptr p -> rev_ptr.(p) <- p :: rev_ptr.(p)
    | _ -> ()
    in
    Array.iter iter v
  | String _ -> ()
  in
  let () = Array.iteri iter mem in
  (** reverse closure *)
  let step visited todo =
    let nvisited = IntSet.union visited todo in
    (** Add every previous element *)
    let fold elt accu =
      let neighbours = rev_ptr.(elt) in
      let fold accu e = if IntSet.mem e nvisited then accu else IntSet.add e accu in
      List.fold_left fold accu neighbours
    in
    let neighbours = IntSet.fold fold todo IntSet.empty in
    (nvisited, neighbours)
  in
  let rec closure visited todo =
    let (nv, nt) = step visited todo in
    if IntSet.is_empty todo then nv
    else closure nv nt
  in
  closure IntSet.empty target

let pr_dot_mem chan (obj, mem) =
  let iter ptr obj =
    Printf.fprintf chan "struct_%08x%a;\n" ptr (pr_dot_obj ptr) obj;
    pr_dot_link (fun ptr -> true) ptr chan obj;
    Printf.fprintf chan "\n";
    pr_atom chan obj;
  in
  Printf.fprintf chan "digraph G {\n";
  Printf.fprintf chan "node[shape=\"record\"]\n";
  Array.iteri iter mem;
  Printf.fprintf chan "\n}\n"
