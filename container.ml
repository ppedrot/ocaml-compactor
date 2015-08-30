open Hopcroft

class type ['a] stack =
object
  inherit ['a] foldable
  method push : 'a -> unit
end

let small = pred (Sys.word_size / 2)
let small_mask = (-1) lsr (small + 1)
let is_small x = 0 < x && x <= small_mask

class int_stack =
object (self)
  val mutable size = 0
  val mutable data = Array.make 8 0
  val mutable data' = []

  method fold : 'r. ('a -> 'r -> 'r) -> 'r -> 'r = fun f accu ->
    let accu = List.fold_left (fun accu x -> f x accu) accu data' in
    let accu = ref accu in
    for i = 0 to pred (size / 2) do
      accu := f (data.(i) lsr small) !accu;
      accu := f (data.(i) land small_mask) !accu;
    done;
    if size mod 2 = 1 then accu := f (data.(size / 2) lsr small) !accu;
    !accu

  method private push_small x =
    let len = Array.length data in
    if size = 2 * len then begin
      let old = data in
      data <- Array.make (2 * len) 0;
      Array.blit old 0 data 0 len;
    end;
    let off = size / 2 in
    if size mod 2 = 0 then
      data.(off) <- x lsl small
    else
      data.(off) <- data.(off) lor x;
    size <- size + 1

  method push x =
    if is_small x then self#push_small x
    else data' <- x :: data'

end

class transition_stack =
object (self)
  val mutable size = 0
  val mutable data = Array.make 8 0
  val mutable data' = []

  method fold : 'r. ('a -> 'r -> 'r) -> 'r -> 'r = fun f accu ->
    let accu = List.fold_left (fun accu x -> f x accu) accu data' in
    let accu = ref accu in
    for i = 0 to pred size do
      let data = data.(i) in
      accu := f { src = data lsr small; dst = data land small_mask } !accu;
    done;
    !accu

  method private push_small { src = x; dst = y } =
    let len = Array.length data in
    if size = len then begin
      let old = data in
      data <- Array.make (2 * len) 0;
      Array.blit old 0 data 0 len;
    end;
    data.(size) <- (x lsl small) lor y;
    size <- size + 1

  method push x =
    if is_small x.src && is_small x.dst then self#push_small x
    else data' <- x :: data'

end
