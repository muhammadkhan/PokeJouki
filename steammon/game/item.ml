open Definitions
open State



(**
This is the module that controls all of the item usage 
And their subsequent effects on steammon
Should the state module be used here? 
*)

(*Using an ether on a pokemon*)		
let use_Ether (p: steammon ref) : unit = 
	p :=  
		{species = (!p).species; 
		 curr_hp = (!p).curr_hp;
		 max_hp = (!p).max_hp; 
		 first_type = (!p).first_type;
		 second_type = (!p).second_type; 
		 first_attack = change_pp_by ((!p).first_attack) (5);
		 second_attack = change_pp_by ((!p).second_attack) (5);
		 third_attack = change_pp_by ((!p).third_attack) (5);
		 fourth_attack = change_pp_by ((!p).fourth_attack) (5);
		 attack = (!p).attack;
		 spl_attack = (!p).spl_attack;
		 defense = (!p).defense;
		 spl_defense = (!p).spl_defense;
		 speed = (!p).speed;
		 status = (!p).status;
		 mods = (!p).mods
		} 

(*using a maxPotion on a pokemon*)	
let use_maxPotion (p : steammon ref) : unit =
  p := 
		{species = (!p).species; 
		 curr_hp = (!p).max_hp;
		 max_hp = (!p).max_hp; 
		 first_type = (!p).first_type;
		 second_type = (!p).second_type; 
		 first_attack = (!p).first_attack;
		 second_attack = (!p).second_attack;
		 third_attack = (!p).third_attack;
		 fourth_attack =  (!p).fourth_attack;
		 attack = (!p).attack;
		 spl_attack = (!p).spl_attack;
		 defense = (!p).defense;
		 spl_defense = (!p).spl_defense;
		 speed = (!p).speed;
		 status = (!p).status;
		 mods = (!p).mods
		}

(*Using a revive on a fainted pokemon*)												
let use_Revive (p : steammon ref) : unit =
  if ((!p).curr_hp = 0) then
		p := 
			{species = (!p).species; 
  		 curr_hp = ((!p).max_hp / 2); (*We revive to half-health *)
  		 max_hp = (!p).max_hp; 
  		 first_type = (!p).first_type;
  		 second_type = (!p).second_type; 
  		 first_attack = (!p).first_attack;
  		 second_attack = (!p).second_attack;
  		 third_attack = (!p).third_attack;
  		 fourth_attack =  (!p).fourth_attack;
  		 attack = (!p).attack;
  		 spl_attack = (!p).spl_attack;
  		 defense = (!p).defense;
  		 spl_defense = (!p).spl_defense;
  		 speed = (!p).speed;
  		 status = (!p).status;
  		 mods = (!p).mods
			}
  else ()
	
(*Using a full heal on a pokemon*)			
let use_FullHeal (p: steammon ref) : unit =
	p := 
		{species = (!p).species; 
		 curr_hp = (!p).curr_hp;
		 max_hp = (!p).max_hp; 
		 first_type = (!p).first_type;
		 second_type = (!p).second_type; 
		 first_attack = (!p).first_attack;
		 second_attack = (!p).second_attack;
		 third_attack = (!p).third_attack;
		 fourth_attack =  (!p).fourth_attack;
		 attack = (!p).attack;
		 spl_attack = (!p).spl_attack;
		 defense = (!p).defense;
		 spl_defense = (!p).spl_defense;
		 speed = (!p).speed;
		 status = [];
		 mods = (!p).mods
		}


(*using one of the X stat-boosting items*)
(*NOT COMPLETED*)
(*This should just increment the specific modifier*)
let use_X_item (i : item) (p : steammon ref) : unit = 
  match i with
	| XAttack -> change_mods_by p 1 1;
	| XSpeed -> change_mods_by p 1 2;
	| XDefense -> change_mods_by p 1 3;
	| XAccuracy -> change_mods_by p 1 4;
	| _ -> ()			  
	