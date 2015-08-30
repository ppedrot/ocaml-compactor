open Hopcroft

class type ['a] stack =
object
  inherit ['a] foldable
  method push : 'a -> unit
end

class int_stack : object inherit [int] stack end
class transition_stack : object inherit [int transition] stack end
