(* Modules *)
(* ML has structures to define modeles *)
(* namespace management *)

(* Syntax *)
(* structure MyModule = struct bindings end *)

(* Call from outside *)
(* ModuleName.bindingName *)

(* Example *)

(* Start module  *)
structure MyMathLib =
struct

fun fact x =
  if x = 0
  then 1
  else x * fact (x - 1)

val half_pi = Math.pi / 2.0

fun doubler y = y + y

end
(* End module *)


    
