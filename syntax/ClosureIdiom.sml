(* Closure Idiom: Combining Functions *)

fun compose(f, g) = fn x => f(g x)

			     
