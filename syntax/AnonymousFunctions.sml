(* Anonymous Functions *)

fun n_times (f, n, x) =
  if n = 0 then x
  else f (n_times(f, n-1, x))

fun triple x = 3 * x

fun triple_n_times1 (n, x) =
  n_times(triple, n, x)

fun triple_n_times2 (n, x) =
  let fun trip y = 3*y
  in
      n_times(trip, n, x)
  end

fun triple_n_times3 (n, x) =
  n_times(let fun trip y = 3*y in trip end, n, x)

(* triple n times with anonymous function *)	 
fun triple_n_times4 (n, x) =
  n_times ((fn x => 3 * x),n ,x)

(* poor style example *)	  
fun nth_tail(n, xs) = n_times((fn y => tl y), n, xs)
