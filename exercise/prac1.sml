(* Functions related to calender dates *)

exception ERROR

(* NO.1 *)
fun is_older ((y1, m1, d1), (y2, m2, d2)) =
  y1 < y2
  orelse (y1 = y2 andalso m1 < m2)
  orelse (y1 = y2 andalso m1 = m2 andalso d1 < d2)

(* NO.2 *)	     
fun number_in_month (dates, month : int) = 
  case dates of
      [] => 0 
    | (_, m, _)::rest => if m = month then 1 + number_in_month (rest, month)
			 else number_in_month (rest, month)

(* NO.3 *)					      
fun number_in_months (dates, months : int list) =
  let fun includes (m) = List.exists (fn x => x = m) months in
      case dates of
	  [] => 0
	| (_, m, _)::rest => if (includes m) then 1 + number_in_months (rest, months)
			     else number_in_months (rest, months)
  end

(* NO.4 *)      
fun dates_in_month (dates, month : int) =
  case dates of
      [] => []
    | (y, m, d)::rest => if m = month then (y, m, d) :: dates_in_month (rest, month)
			 else dates_in_month (rest, month)

(* NO.5 *)					     
fun dates_in_months (dates, months : int list) =
  let fun includes (m) = List.exists (fn x => x = m) months in
      case dates of
	  [] => []
	| (y, m, d)::rest => if (includes m) then (y, m, d) :: dates_in_months (rest, months)
			     else dates_in_months (rest, months)					     
  end

(* NO.6 *)
fun get_nth ([], _) = raise ERROR 
  | get_nth (s::xs, n) = if n = 1 then s else get_nth(xs, n-1)

(* NO.7 *)						     
fun date_to_string (y, m, d) =
  let
      val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "Octorber", "Norvember", "December"]
  in
      get_nth(months, m) ^ " " ^ (Int.toString d) ^ ", " ^ (Int.toString y)
  end  

(* NO.8 *)      
fun number_before_reaching_sum (sum, []) = raise ERROR 
  | number_before_reaching_sum (sum, x::xs) = if sum - x <= 0 then 0
					      else 1 +  number_before_reaching_sum (sum - x, xs)

(* NO.9 *)										   
fun what_month (d) =
  let val days = [31,28,31,30,31,30,31,31,30,31,30,31] in
      number_before_reaching_sum (d, days) + 1
  end

(* NO.10 *)      
fun month_range (day1, day2) =
  let fun countup (x, y) = if x-1 = y then [] else x :: countup(x + 1, y) in
      countup(what_month(day1), what_month(day2))
  end

(* NO.11 *)
fun oldest ([]) = NONE 
  | oldest (dates) =
    let 
	fun get_oldest(dates: (int*int*int) list) =
	  case dates of
	     [] => raise ERROR
	    | x::y::[] => if is_older(x, y) then x else y 
            | x::rest  => let val rest_oldest = get_oldest(rest)
			  in
			      if is_older(x, rest_oldest) then x else rest_oldest
			  end
    in
	SOME (get_oldest dates)
    end 

