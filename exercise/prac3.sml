(* provided *)
exception NoAnswer

(* 1 *)
val only_capitals =
  List.filter (fn s => Char.isUpper(String.sub(s, 0))) 

(* 2 *)
val longest_string1 =
  List.foldl (fn (x,y) => if String.size x > String.size y then x else y) ""
	     

(* 3 *)	       
val longest_string2 =
  List.foldl (fn (x,y) => if String.size x >= String.size y then x else y) ""
	     
	       
(* 4 *)
fun longest_string_helper f s strs =
  List.foldl (fn (x,y) => f(x,y)) s strs

	     
val longest_string3 = longest_string_helper
			  (fn (x,y) => if String.size x > String.size y then x else y)
			  ""
			  

val longest_string4 = longest_string_helper
			  (fn (x,y) => if String.size x >= String.size y then x else y)
			  ""
			  
(* 5 *)
val longest_capital = longest_string4 o only_capitals

(* 6 *)
val rev_string = String.implode o List.rev o String.explode

(* 7 *)
fun first_answer f lst =
  case lst of
      [] => raise NoAnswer
    | x::xs => case f x of
		   SOME z => z
		 | NONE => first_answer f xs
				
(* 8 *)
fun all_answers f lst =
  let
      fun acc_helper ([], acc) = SOME acc
	| acc_helper (x::xs, acc) =
	  case f x of
	      NONE => NONE
	    | SOME z => acc_helper (xs, acc @ z) 
  in
      acc_helper (lst, [])
  end

(* 9 - provided *)      
datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end
      
(* 9-a *)
fun count_wildcards p =
  g (fn x => 1) (fn x => 0) p

(* 9-b *)
fun count_wild_and_variable_lengths p =
  g (fn x => 1) (fn s => String.size s) p    

(* 9-c *)
fun count_some_var (s, p) =
  g (fn x => 0) (fn x => if x = s then 1 else 0) p

(* 10 *)
fun check_pat p =
  let
      fun acc_helper (p, acc) =
	case p of
	    Variable x        => x::acc
	  | TupleP ps         => List.foldl (fn (p,i) => acc_helper(p, acc) @ i) [] ps
	  | ConstructorP(_,p) => acc_helper(p, acc)
	  | _                 => acc

      fun is_distinct (lst : string list) =
	case lst of
	    x::xs => (not (List.exists (fn e => e = x) xs)) andalso (is_distinct xs)
	  | _     => true  				     
  in
      is_distinct (acc_helper (p, []))
  end

(* 11 *) (* (valu, pattern) -> (string, value) list option *)
fun match (v, p) =
  case (v, p) of
      (_, Wildcard) => SOME []
    | (Const e, Variable s) => SOME [(s, Const e)]
    | (Unit, UnitP) => SOME []
    | (Const e1, ConstP e2) => if e1 = e2 then SOME [] else NONE
    | (Tuple vx, TupleP px) => if (List.length vx) = (List.length px)
			       then all_answers match (ListPair.zip (vx, px))
			       else NONE
    | (Constructor (s1, v), ConstructorP (s2, p)) => if s1 = s2
						     then match(v, p) 
						     else NONE 
    | (_, _) => NONE   

(* 12 *)
fun first_match v ps = SOME (first_answer match (List.map (fn p => (v, p)) ps))
		       handle _ => NONE				       
