(* First Class Functions *)
(* Using functions as values *)

fun double x = 2*x
fun incr x = x + 1
		     
val a_tuple = (double, incr, double(incr 7))
val eighteen = (#1 a_tuple) 9
val eleven = (#2 a_tuple) 10			    

(* ex *)
fun increment_n_times_lame (n, x) =
  if n = 0 then x
  else 1 + increment_n_times_lame (n-1, x)

fun double_n_times_lame (n, x) =
  if n = 0 then x
  else 2 + double_n_times_lame(n-1, x)

fun nth_tail_lame (n, xs) =
  if n = 0 then xs
  else tl (nth_tail_lame(n-1, xs))

(* ex  *)
(* ('a -> 'a) * int * 'a -> 'a *)	  
fun n_times (f, n, x) =
  if n = 0 then x
  else n_times(f, n-1, f(x))

val x1 = n_times(double, 4, 7)
val x2 = n_times(incr, 4, 7)
val x3 = n_times(tl, 2, [3,4,5,6,7])		

fun addition(n, x) = n_times(increment, n, x)
fun double_n_times(n, x) = n_times(double, n, x)
fun nth_tail(n, x) = n_times(tl, n, x)

fun triple x = 3 * x
fun triple_n_times (n, x) = n_times(triple, n, x)		       
		
(* higher-order function, but not polymorphic *)
fun times_until_zero (f, x) =
  if x = 0 then 0 else 1 + times_until_zero(f, f x)

(* polymorphic, but not higher-order *)					   
fun len xs =
  case xs of
      [] => 0 
   | x::xs' => 1 + len xs' 
