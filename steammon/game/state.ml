open Definitions
open Util

(**
This State module will be used to keep track of the state of the game
This includes using items, attacking, switching pokemon, keeping track
of properties like HP, PP, inventory, etc *)

let get_hp (p : steammon) = p.curr_hp

let change_hp_by (p : steammon) (delta : int) : steammon = {
	species = p.species;
	(*|||||||||||||||||||||||||||||||||||||*)
	curr_hp = (let x = p.curr_hp + delta in if x < 0 then 0 else x);
	(*|||||||||||||||||||||||||||||||||||||*)
	max_hp = p.max_hp; first_type = p.first_type; second_type = p.second_type;
	first_attack = p.first_attack; second_attack = p.second_attack;
	third_attack = p.third_attack; fourth_attack = p.fourth_attack;
	attack = p.attack; spl_attack = p.spl_attack; defense = p.defense;
	spl_defense = p.spl_defense; speed = p.speed; status = p.status;
	mods = p.mods
}
	
let get_pp (a : attack) = a.pp_remaining
	
let change_pp_by (a : attack ref) (delta : int) : unit =
	a := {
	name = !a.name; element = !a.element; max_pp = !a.max_pp;
	(*|||||||||||||||||||||||||||||||||||||*)
	pp_remaining = (!a.pp_remaining + delta);
	(*|||||||||||||||||||||||||||||||||||||*)
	power = !a.power; accuracy = !a.accuracy;
	crit_chance = !a.crit_chance; effect = !a.effect
  }

(*i = 1 changes attack, 2 speed, 3 defense, 4 accuracy*)
let change_mods_by (p : steammon ref) (n : int) (i : int) : unit =
  let change_mods (m : modifier) (nn : int) (i : int): modifier = 
	  let n = if nn > 3 then 3 else if n < (-3) then (-3) else nn in
		match i with
	  | 1 -> 
  		{attack_mod = n;
  		 speed_mod = m.speed_mod;
  		 defense_mod = m.defense_mod;
  		 accuracy_mod = m.accuracy_mod;	
  		} 
	  | 2 ->
  		{attack_mod = m.attack_mod;
  		 speed_mod = n;
  		 defense_mod = m.defense_mod;
  		 accuracy_mod = m.accuracy_mod;	
  		} 
	  | 3 ->
  		{attack_mod = m.attack_mod;
  		 speed_mod = m.speed_mod;
  		 defense_mod = n;
  		 accuracy_mod = m.accuracy_mod;	
  		} 
	  | 4 ->
  		{attack_mod = m.attack_mod;
  		 speed_mod = m.speed_mod;
  		 defense_mod = m.defense_mod;
  		 accuracy_mod = n;	
  		} 
	  | _ -> m 
	in
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
		 status = !p.status;
		 mods = change_mods (!p).mods n i;
		}	 	

let change_status_list (p : steammon) (ss: status list) : steammon = {
	species = p.species; curr_hp = p.curr_hp; max_hp = p.max_hp;
	first_type = p.first_type; second_type = p.second_type;
	first_attack = p.first_attack; second_attack = p.second_attack;
	third_attack = p.third_attack; fourth_attack = p.fourth_attack;
	attack = p.attack; spl_attack = p.spl_attack; defense = p.defense;
	spl_defense = p.spl_defense; speed = p.speed;
	(*|||||||||||||||||||||||||||||||||||||*)
	status = ss;
	(*|||||||||||||||||||||||||||||||||||||*)
	mods = p.mods
}

let change_speed (p : steammon) (ns : int) : steammon = {
	species = p.species; curr_hp = p.curr_hp; max_hp = p.max_hp;
	first_type = p.first_type; second_type = p.second_type;
	first_attack = p.first_attack; second_attack = p.second_attack;
	third_attack = p.third_attack; fourth_attack = p.fourth_attack;
	attack = p.attack; spl_attack = p.spl_attack; defense = p.defense;
	spl_defense = p.spl_defense;
	(*|||||||||||||||||||||||||||||||||||||*)
	speed = ns;
	(*|||||||||||||||||||||||||||||||||||||*)
	status = p.status;
	mods = p.mods
}

(*Based on what i is, we decrement the inventory accordingly in order*)
(*i can be from 0 to 7 (inclusive) *)
let change_inventory (iv : int ref list) (delta : int) (i : int) : unit = 
	if i >= 0 && i <= 7 then
		(List.nth iv i) := !(List.nth iv i) + delta
	else
		failwith "not a valid option"
		
let all_are_dead (sl : steammon list) : bool = 
	if sl = [] then false
	else				
		let rec helper (s : steammon list) : bool =
	    match s with
		  | [] -> true
		  | h::t -> h.curr_hp = 0 && helper t
	  in 
		helper sl	

let update_steammon_attack (p : steammon ref) (a : attack) (pos : int) : unit = (*pos = 1, 2, 3, 4 only*)
  p := 
		{species = (!p).species; 
		 curr_hp = (!p).curr_hp;
		 max_hp = (!p).max_hp; 
		 first_type = (!p).first_type;
		 second_type = (!p).second_type; 
		 first_attack = if pos = 1 then a else (!p).first_attack;
		 second_attack = if pos = 2 then a else (!p).second_attack;
		 third_attack = if pos = 3 then a else (!p).third_attack;
		 fourth_attack =  if pos = 4 then a else (!p).fourth_attack;
		 attack = (!p).attack;
		 spl_attack = (!p).spl_attack;
		 defense = (!p).defense;
		 spl_defense = (!p).spl_defense;
		 speed = (!p).speed;
		 status = !p.status;
		 mods = !p.mods;
		}	

(*--------------Extra Utility Functions--------------*)
let deref_list (lst : 'a ref list) : 'a list = List.map (fun x -> !x) lst

let reref_list (lst : 'a list) : 'a ref list = List.map (fun x -> ref x) lst

let mod_constant_atk (i : int) : float =
	match i with
		| 1 -> Constants.cATTACK_UP1
		| 2 -> Constants.cATTACK_UP2
		| 3 -> Constants.cATTACK_UP3
		| (-1) -> Constants.cATTACK_DOWN1
		| (-2) -> Constants.cATTACK_DOWN2
		| (-3) -> Constants.cATTACK_DOWN3
		| _ -> 1.

let mod_constant_def (i : int) : float =
	match i with
		| 1 -> Constants.cDEFENSE_UP1
		| 2 -> Constants.cDEFENSE_UP2
		| 3 -> Constants.cDEFENSE_UP3
		| (-1) -> Constants.cDEFENSE_DOWN1
		| (-2) -> Constants.cDEFENSE_DOWN2
		| (-3) -> Constants.cDEFENSE_DOWN3
		| _ -> 1.

let mod_constant_spd (i : int) : float =
	match i with
		| 1 -> Constants.cSPEED_UP1
		| 2 -> Constants.cSPEED_UP2
		| 3 -> Constants.cSPEED_UP3
		| (-1) -> Constants.cSPEED_DOWN1
		| (-2) -> Constants.cSPEED_DOWN2
		| (-3) -> Constants.cSPEED_DOWN3
		| _ -> 1.

let mod_constant_acc (i : int) : float =
	match i with
		| 1 -> Constants.cACCURACY_UP1
		| 2 -> Constants.cACCURACY_UP2
		| 3 -> Constants.cACCURACY_UP3
		| (-1) -> Constants.cACCURACY_DOWN1
		| (-2) -> Constants.cACCURACY_DOWN2
		| (-3) -> Constants.cACCURACY_DOWN3
		| _ -> 1.
