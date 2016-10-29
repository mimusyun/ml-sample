(* Function Patterns *)

(* Syntax *)
(*
fun f p1 = e1 
  | f p2 = e2 
    ...
  | f pn = en 
*)

datatype exp =
	 Constant of int
	 | Negate of exp
	 | Add of exp * exp
	 | Multiple of exp * exp

fun eval (Constant i) = i 
  | eval (Negate e2) = ~ (eval e2) 
  | eval (Add(e1, e2)) = (eval e1) + (eval e2) 
  | eval (Multiple(e1, e2)) = (eval e1) * (eval e2)

fun append ([], ys) = ys 
  | append (x::xs', ys) = x :: append(xs',ys) 					      
