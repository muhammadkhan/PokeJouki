
open Definitions
(**
This is the module that controls all of the item usage 
And their subsequent effects on steammon
Should the state module be used here? 
*)

(*Do we create a separate module that handles all of the effects?*)

(*At this point we are assuming that all of the fields are mutable*)

(*Using an ether on a pokemon*)		
let use_Ether (i : item) (p: steammon) : unit = 
	if (!(List.nth inventory 0) <> 0) then
	 (p.first_attack.pp_remaining := !(p.first_attack.pp_remaining) + 5;
		p.second_attack.pp_remaining := !(p.second_attack.pp_remaining) + 5;
		p.third_attack.pp_remaining := !(p.third_attack.pp_remaining) + 5;
		p.fourth_attack.pp_remaining := !(p.fourth_attack.pp_remaining) + 5;
		List.nth inventory 0 := !(List.nth inventory 0) - 1;	
		)
	else ()	

(*using a maxPotion on a pokemon*)	
let use_maxPotion (i : item) (p : steammon) : unit =	
	(*first check if we have some to use*)
	if (!(List.nth inventory 1) <> 0) then
		(if (!p.curr_hp <> 0) then	 
	    (p.curr_hp := p.max_hp; 
	    (List.nth inventory 2) := !(List.nth inventory 2) - 1;
		  )
		 else ()	
		)
	else ()
						
(*Using a revive on a fainted pokemon*)												
let use_Revive (i : item) (p : steammon) : unit = 
  if (!(List.nth inventory 2) <> 0) then
		(if !p.curr_hp = 0 then
			(p.curr_hp := p.max_hp / 2;
			 List.nth inventory 2 := !(List.nth inventory 2) - 1; 
			)
		 else ();	
		)
	else ()
	
(*Using a full heal on a pokemon*)			
let use_FullHeal (i : item) (p: steammon) : unit = 
	if (!(List.nth inventory 3) <> 0) then
		(p.status := []; 
		 List.nth inventory 3 := !(List.nth inventory 3) - 1;
		)
	else ()					

(*using one of the X stat-boosting items*)
(*NOT COMPLETED*)
(*This should just increment the specific modifier*)
let use_X_item (i : item) : unit = 	()






										
		
				