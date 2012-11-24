open Definitions
open State
open Constants

(**
This is the module that handles status effects
but not effects such as self-attacks

*)

(*This occurs at the end of the turn *)
(*We need to make sure that this occurs at the end of the turn*)
(*If the steammon is frozen, it cannot attack that turn*)
let frozen_effect (p : steammon ref) : unit = 
	let probability = Random.int 100 in
	if probability < cDEFROST_CHANCE then
		p := 
			{species = (!p).species; 
  		 curr_hp = (!p).curr_hp; (*We revive to half-health *)
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
  		 status = List.filter (fun x -> x <> Frozen) (!p).status;
  		 mods = (!p).mods
			}
	else () 

(*Poison damage that occurs at the end of every turn if the pokemon is poisoned*)
let poison_damage (p : steammon ref) : unit = 
	p := 
		{species = (!p).species; 
  	 curr_hp = 
			 (!p).curr_hp - (int_of_float ((cPOISON_DAMAGE *. (float_of_int (!p).max_hp)))); (*We revive to half-health *)
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
		
(*If the pokemon is sleeping, then we will potentially wake it up*)	
(*It should be able to attack after its waking up though  *)					
let wakeup (p : steammon ref) : unit = 
	let probability = Random.int 100 in 
	 if (probability < cWAKE_UP_CHANCE) then
		p := 
			{species = (!p).species; 
  		 curr_hp = (!p).curr_hp; (*We revive to half-health *)
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
  		 status = List.filter (fun x -> x <> Asleep) (!p).status;
  		 mods = (!p).mods			
			}
	 else ()
		
(*This is the one-time slowdown that happens due to paralysis*)
(*Note : make sure that paralysis is already in the status list*)						
let paralyzed_slowdown (p : steammon ref) : unit = 
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
  	 speed = (!p).speed / cPARALYSIS_SLOW;
  	 status = (!p).status;
  	 mods = (!p).mods
		}
				
(*This will determine whether the paralyzed pokemon can attack or not*)		
let paralyze_attack : bool = 
	let probability = Random.int 100 in 
	probability < cPARALYSIS_CHANCE
	
(*This occurs before the chance to attack*)																												
let snap_out_confusion : bool  =
	let probability = Random.int 100 in 
	probability < cSNAP_OUT_OF_CONFUSION				
										
(*This occurs if the pokemon does not snap out of confusion. *)																		
let confused_self_attack : bool = 
	let probability = Random.int 100 in 
	probability < cSELF_ATTACK_CHANCE

(*create a separate confused attack within the attack module *)																										