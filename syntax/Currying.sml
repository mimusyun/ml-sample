(* Currying *)

fun sorted3_tuple (x, y, z) = z >= y andalso y >= x

val t1 = sorted3_tuple(7,9,11)						      

(* new way - currying *)
val sorted3 = fn x => fn y => fn z =>
		 z >= y andalso y >= x

(* fun sorted3 x = fn y => fn z => ... *)							      
val t2 = (((sorted3 7) 9) 11)

val t3 = sorted3 7 9 11

(* val wrong1 = sorted3_tuple 7 9 11 *)		 
(* val wrong2 = sorted3 (7, 9, 11) *)


(* ex *)		 
fun fold f acc xs = (* fun fold f = fn acc => f xs => *)
  case xs of
      [] => acc
   | x::xs => fold f (f(acc, x)) xs   

fun sum xs = fold (fn (x,y) => x+y) 0 xs 		   

(* Unnecessary function wrapping *)
fun sum2_inferior xs = fold (fn (x,y) => x+y) 0 xs

val sum2 = fold (fn (x,y) => x+y) 0
