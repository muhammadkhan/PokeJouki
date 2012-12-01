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
		};
  Netgraphics.add_update(Message(!p.species ^ " used Ether!")) 

(*using a maxPotion on a pokemon*)	
let use_maxPotion (p : steammon ref) : unit =
	if !p.curr_hp = 0 then () 
	else
		(Netgraphics.add_update(Message(!p.species ^ " used MaxPotion!"));
    	p := change_hp_by (!p) (!p.max_hp - !p.curr_hp))

(*Using a revive on a fainted pokemon*)
(*Eliminate all status effects*)												
let use_Revive (p : steammon ref) : unit =
  if ((!p).curr_hp = 0) then
		(Status.unparalyze p;
		p := change_status_list (!p) [];
		p := change_hp_by (!p) (!p.max_hp / 2);
		Netgraphics.add_update(Message(!p.species ^ " was revived!")))
  else failwith "pokemon was not fainted"
	
(*Using a full heal on a pokemon*)			
let use_FullHeal (p: steammon ref) : unit =
	Status.unparalyze p;
	Netgraphics.add_update(Message(!p.species ^ " used FullHeal!"));
	p := change_status_list (!p) []

(*using one of the X stat-boosting items*)
(*This should just increment the specific modifier*)
let use_X_item (i : item) (p : steammon ref) : unit = 
  match i with
	| XAttack -> change_mods_by p 1 1;
	| XSpeed -> change_mods_by p 1 2;
	| XDefense -> change_mods_by p 1 3;
	| XAccuracy -> change_mods_by p 1 4;
	| _ -> ()			  
	