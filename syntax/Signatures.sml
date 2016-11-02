(* Signatures *)
(* A signature is a type for a module *)

(* Syntax *)
(*
signature SIGNATURE_NAME
sig
...
(types for bindings)
...
end
*)

(* 
structure MyModule :> SIGNAME
struct bindings end
*)

(* Example *)
(*
signature MATHLIB =
sig 
val fact : int -> int
val half_pi : real
(*val doubler : int -> int*)
end

structure MyMathLib :> MATHLIB =
struct
fun fact x = 0
val half_pi = Math.pi/2.0
fun doubler x = x * 2
end
*)    


(* The bindings defined in a signature must be impletemented in a structure with the signature. The structure can have other functions not defined in a signature, but they cannot be called from outside. In the example above, you cannot call doubler from outside. *)

(* Example *)
structure Rational1 =
struct

datatype rational = Whole of int | Frac of int*int
exception BadFrac

fun gcd (x, y) =
  if x = y
  then x
  else if x < y
  then gcd(x, y-x)
  else gcd(y, x)
	      
fun reduce r =
  case r of
      Whole _ => r
    | Frac(x, y) =>
      if x = 0
      then Whole 0
      else let val d = gcd(abs x, y) in
	       if d = y
	       then Whole(x div d)
	       else Frac(x div d, y div d)
	   end	      
	       
fun make_frac (x, y) =
  if y = 0
  then raise BadFrac
  else if y < 0
  then reduce(Frac(~x, ~y))
  else reduce(Frac(x, y))

fun add (r1, r2) =
  case (r1, r2) of
      (Whole(i), Whole(j)) => Whole(i+j)
    | (Whole(i), Frac(j, k)) => Frac(j+j*i, k)
    | (Frac(j, k), Whole(i)) => Frac(j+k*i, k)
    | (Frac(a,b), Frac(c,d)) => reduce (Frac(a*d + b*c, b*d))

fun toString r =
  case r of
      Whole i => Int.toString i
    | Frac (a, b) => (Int.toString a) ^ "/" ^ (Int.toString b)
	     
end	
    
    
