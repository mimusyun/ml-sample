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
fun all_except_option (s: string, strs: string list) =
  let
      fun get_except_first ([]) = []
	| get_except_first (x::xs) =
	  if x <> s andalso not (List.exists (fn e => e = s) xs) then []
	  else if x = s then xs
	  else x :: get_except_first(xs)
  in
      case get_except_first (strs) of
	  [] => NONE
	| xs => SOME xs
  end	   	   

(* 1-b *)
fun get_substitutions1 ([], _) = []
  | get_substitutions1 (x::xs, s : string) =
    let
	fun get_value (strs) =
	  case all_except_option (s, strs) of
	      NONE => []
	    | SOME xs => xs 
    in
	get_value(x) @ get_substitutions1 (xs, s)
    end

(* 1-c *) (* Tail Recursion *)	
fun get_substitutions2 ([], _) = []
  | get_substitutions2 (strs, s : string) =
    let
	fun get_value (strs) =
	  case all_except_option (s, strs) of
	      NONE => []
	    | SOME xs => xs 									 
									  
	fun acc_helper ([], acc) = acc
	  | acc_helper (x::xs, acc) =
	    acc_helper (xs, acc @ get_value(x))
    in
	acc_helper(strs, [])
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
fun remove_card (cs, c: card, e) =
  let
      fun get_except_first ([]) = [] 
	  | get_except_first (x::xs) =
	    if x <> c andalso not (List.exists (fn e => e = c) xs) then raise e
	    else if x = c then xs
            else x :: get_except_first(xs)
  in
      get_except_first (cs)
  end

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

(* 2-g *)
(* GAMEEND - user chooses no move / sum of the hc is greater than the goal *)      
(* MOVE - drawing - get a card from cs, and add it to hc / discarding - choose a card from hc to remove *)      
(*
fun officiate (cs, ms, goal) =
  let
      fun run (cs, ms, goal, hs) = ???
      (* discard -> play continues with held-cards not having and cs unchanged *)
      fun discard (cards, c) = remove (cards, c, IllegalMove)

      (* draw -> if cs empty = gameover 
                 elif sum(held_cards)>goal = gameover 
                 else play continues  *)
      fun draw (cs) =
	case cs of
	    []   => score (held_cards) 
	  | x::xs => if sum_cards (help_cards) > goal then score (held_cards)
		     else run (xs,)					   
      (* discard c = remove (held-cards, c, e) *)
*)
	
