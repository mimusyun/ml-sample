(* Practice 2 *)

fun same_string(s1 : string, s2 : string) =
  s1 = s2

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception ERROR				      
exception IllegalMove	   

	      
(* 1-a *)     

fun all_except_option (_, []) = NONE 
  | all_except_option (s, x::xs) =
    if same_string (s, x) then SOME xs
    else case all_except_option (s, xs) of
	     NONE => NONE 
	   | SOME xs => SOME (x::xs)
				   

(* 1-b *)
fun get_substitutions1 ([], _) = [] 
  | get_substitutions1 (xs::xss, s) =
    case all_except_option (s, xs) of
	NONE => get_substitutions1(xss, s)
      | SOME lst => lst @ get_substitutions1(xss, s)

(* 1-c *) (* Tail Recursion *)
fun get_substitutions2 ([], _) = [] 
  | get_substitutions2 (xs::xss, s) =
    let
	fun acc_helper ([], acc) = acc 
	  | acc_helper (xs::xss, acc) =
	    case all_except_option (s, xs) of
		NONE => acc_helper (xss, acc)
	      | SOME xs => acc_helper (xss, acc @ xs) 
    in
	acc_helper (xs::xss, [])
    end

(* 1-d *)
fun similar_names (strs, {first=f, middle=m, last=l}) =
  {first=f, middle=m, last=l} :: (List.map (fn e => {first=e, middle=m, last=l}) (get_substitutions2 (strs, f)))

(* 2-a *)
fun card_color (card) =
  case card of
      (Clubs, _) => Black
    | (Diamonds, _) => Red 
    | (Hearts, _) => Red 
    | (Spades, _) => Black

(* 2-b *)
fun card_value (card) =
  case card of
      (_, Ace) => 11
    | (_, Jack) => 10 
    | (_, King) => 10
    | (_, Queen) => 10
    | (_, Num n) => n

(* 2-c *)
fun remove_card ([], _ , e) = raise e
  | remove_card (x::xs, c : card, e) =
    if x = c then xs
    else x :: remove_card(xs, c, e)

(* 2-d *)
fun all_same_color (cs) =
  let
      fun get_first_color ([]) = raise ERROR
	| get_first_color (x::xs) = card_color (x)
  in
      case cs of
	  []       => true
	| x::[]    => true
	| x::y::[] => (card_color (x) = card_color (y))
	| x::y::ys => if (card_color (y) = get_first_color (ys)) then all_same_color(ys) else false
  end

(* 2-d *)
fun all_same_color ([]) = true 
  | all_same_color (x::[]) = true 
  | all_same_color (x::y::ys) = card_color(x) = card_color(y) andalso all_same_color(y::ys) 

(* 2-e *)      
fun sum_cards (cs) =
  let
    fun acc_helper ([], acc) = acc 
      | acc_helper (x::xs, acc) = acc_helper(xs, acc + x)
  in
      acc_helper((List.map (fn x => card_value(x)) cs), 0)
  end    

(* 2-f *)
fun score (cs, goal) =
  let 
      fun preliminary_score (held_cards) =
	let val sum = sum_cards (held_cards) in
	    if sum > goal then 3 * (sum - goal)
	    else goal - sum
	end

      fun get_score (held_cards) =
	if all_same_color (held_cards) then preliminary_score (held_cards) div 2
	else preliminary_score (held_cards)			      
  in
      get_score (cs)
  end

	
