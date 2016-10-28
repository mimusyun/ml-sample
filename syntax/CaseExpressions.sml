(* Case Expressions *)

(* Syntax *)
(* 
   case e0 of
        p1 => e1
        p2 => e2
        ...
        pn => en
*)

datatype mytype = TwoInts of int * int (* int * int -> mytype *)
	        | Str of string (* string -> mytype *)
                | Pizza (* mytype *)

		      
(* mytype -> int *)
fun f (x : mytype) =
  case x of
      Pizza => 3 
    | Str s => String.size s
    | TwoInts(i1, i2) => i1 + i2  		      
(*  | Pizza - Redundant causes an error  *)

(* fun g (x : mytype) = case x of Pizza => 10  *)				  
(* Missing patterns causes warning... *)
