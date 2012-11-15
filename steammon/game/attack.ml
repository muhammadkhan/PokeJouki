open Definitions
open Util


(**
This is the module that handles when a pokemon is attacked by a certain type of attack. 
This will evaulate an attack from a pokemon to another pokemon. 
*)

(*The util module has the multiplier values for atk type and defend type*)

(**given an attack on steammon p, 
find the multiplier associated with the attack*)
let find_atk_mult (atk : attack) (p : steammon) : double = 
	let (p_typ1, p_typ2, atk_type) = 
		(p.first_type, p.second_type, atk.element) in 
		 match p_typ1, p_typ2 with
			| Some x , Some y -> 
				(weakness atk_type x) *. (weakness atk_type y)
	    | Some x , None ->
				weakness atk_type x
			| None, _ -> failwith "Steammon must have initial type"	
	
	 





