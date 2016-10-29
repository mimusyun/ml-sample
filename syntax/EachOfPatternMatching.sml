(* Pattern Matching is also available for each-of types *)
(* Below are the poor style examples *)
fun sum_triple triple =
  case triple of
      (x, y, z) => x + y + z

fun full_name r =
  case r of
      {first=x, middle=y, last=z} => x ^ " " ^ y " " ^ z

(* Better example *)							   
fun sum_triple2 triple =
  let val (x, y, z) = triple
  in
      x + y + x
  end

fun full_name2 r =
  let val {first=x, middle=y, last=z} = r
  in
      x ^ " " ^ y " " ^ z
  end

(* Function-argument patterns *)
fun sum_triple3 (x, y, z) =
  x + y + z

fun full_name3 {first=x, middle=y, last=z} =
  x ^ " " ^ y " " ^ z

fun rotate_left (x, y, z) = (y, z, x)
