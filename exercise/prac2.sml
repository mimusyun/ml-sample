(* Practice 2 *)

(* No.1 *)
fun all_except_option (str : string, strs) =
  if List.exists (fn x => x = str) strs
  then SOME (List.filter (fn x => x <> str) strs)
  else NONE

(* No.2 *)
fun get_substitutions1 ([], _) = []
  | get_substitutions1 (x::xs, s : string) =
    let
	fun get_except_first ([]) = [] 
	  | get_except_first (x::xs) =
	    if x <> s andalso not (List.exists (fn e => e = s) xs) then []
	    else if x = s then xs
            else x :: get_except_first(xs)
    in
	get_except_first(x) @ get_substitutions1 (xs, s)
    end

(* No.3 *) (* Tail Recursion *)	
fun get_substitutions2 ([], _) = []
  | get_substitutions2 (strs, s : string) =
    let
	fun get_except_first ([]) = [] 
	  | get_except_first (x::xs) =
	    if x <> s andalso not (List.exists (fn e => e = s) xs) then []
	    else if x = s then xs
            else x :: get_except_first(xs)									 
									  
	fun acc_helper ([], acc) = acc
	  | acc_helper (x::xs, acc) =
	    acc_helper (xs, acc @ get_except_first(x))
    in
	acc_helper(strs, [])
    end

(* No.4 *)
fun similar_names (strs, {first=f, middle=m, last=l}) =
  {first=f, middle=m, last=l} :: (List.map (fn e => {first=e, middle=m, last=l}) (get_substitutions2 (strs, f)))
