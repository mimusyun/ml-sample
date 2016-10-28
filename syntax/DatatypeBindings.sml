(* Datatype bindings *)
(* Adds a new type to the environment *)

datatype mytype = TwoInts of int * int (* int * int -> mytype *)
	        | Str of string (* string -> mytype *)
                | Pizza (* mytype *)

val a = Str "hi"
val b = Str
val c = Pizza
val d = TwoInts(3,7)
val e = a	       
val f = Str(if true then "hi" else "bye")

(* Other examples - a bit more useful ones *)	   
datatype suit = Club | Diamond | heart | Spade
					     
datatype rand = Jack | Queen | King | Ace | Num of int

datatype id = StudentNum of int
            | Name of string * (string option) * string


(* example - Expression Trees *)
datatype exp = Constant of int
	     | Negate   of exp
	     | Add      of exp * exp
             | Multiply of exp * exp 

val exp = Add (Constant (10+9), Negate (Constant 4))				     

fun eval e =
  case e of
      Constant i => i
   |  Negate e2 => ~ (eval e2) 
   |  Add(e1,e2) => (eval e1) + (eval e2) 
   |  Multiply(e1,e2) => (eval e1) * (eval e2)

val ans = eval exp;					 
(* eval exp; will be 15 *) 					 

fun number_of_adds e =
  case e of
      Constant i => 0
   |  Negate e2 => number_of_adds e2 
   |  Add(e1,e2) => 1 + (number_of_adds e1) + (number_of_adds e2) 
   |  Multiply(e1,e2) => (eval e1) * (number_of_adds e2)
