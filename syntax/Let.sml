(* Let Definition *)
(* Local bindings *)

(* Syntax *)
(* let b1 b2 ... bn in e end *)

fun silly1 (z : int) =
  let
      val x = if z > 0 then z else 34
      val y = x + z + 9 
  in
      if x > y then x * 2 else y * y
  end

fun silly2 () =
  let
      val x = 1
  in
      (let val x = 2 in x + 1 end) + (let val y = x + 2 in y+1 end)
  end

fun silly3 () =
  let
      val x = (let val x = 5 in x + 10 end)
  in
      (
	x,
	let val x = 2 in x end,
	let val x = 10 in
	    let val x = 2 in x end
	end
      )
  end

fun countup_from1 (x : int) =
  let
      fun count (from : int) =
	if from = x
	then x :: []
	else from :: count(from+1)
  in
      count(1)
  end
