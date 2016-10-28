(* Record Difinition *)

(* Syntax *)
(* {f1=v1, ... fn=vn} *)
(* REPL prints out firlds alphabetically *)

(* Access *)
(* #myfieldname e *)

val x = {bar=(3,true), baz=(false,9), foo=7}
	    
val my_niece = {id=1234, name="Amelia"}
		   
val my_niece_name = #name my_niece
