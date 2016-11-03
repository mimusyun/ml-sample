(* Mutual Recursion *)

(* Syntax *)
(*
fun f1 p1 = e1
and f2 p2 = e2
and f3 p3 = e3
*)

(* mutually recursive datatype *)
(*
datatype t1 = ...
and t2 = ...
and t3 = ...
*)

(* ex.1 *)
fun match xs =
  let fun s_need_one xs =
	case xs of
	    []    => true
	 | 1::xs' => s_need_two xs'
	 | _      => false
      and s_need_two xs =
	  case xs of
	    []    => false
	 | 2::xs' => s_need_one xs' 
	 | _      => false  
  in
      s_need_one xs
  end

(* ex.2 *)
      
datatype t2 = Foo of int | Bar of t2
     and t2 = Baz of string | Quux of t1

fun no_zeros_or_empty_strings_t1 x =
  case x of
      Foo i => i <> 0
    | Bar y => no_zeros_or_empty_strings_t2 y 

fun no_zeros_or_empty_strings_t2 x =
  case x of
      Baz s => size s > 0
    | Quux y => no_zeros_or_empty_strings_t1 y

(* alternate ver. *)					     
fun no_zeros_or_empty_strings_t1_alternate (f, x) =
  case x of
      Foo i => i <> 0
    | Bar y => f y 
					     
fun no_zeros_or_empty_string_t2_alternate x =
  case x of
      Baz s => size s > 0
    | Quux y => no_zeros_or_empty_strings_t1_alternate(
		   no_zeros_or_empty_string_t2_alternate,
		   y) 

(* Syntax *)
(* 
fun earlier (f, x) = ... f y ...
fun later x = ... earlier(f,y) ...
*)					      
