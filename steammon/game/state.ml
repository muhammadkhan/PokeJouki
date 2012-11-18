open Definitions
open Util
open Attack
(*This State module will be used to keep track of the state of the game*)
(*This includes using items, attacking, switching pokemon, keeping track*)
(*of properties like HP, PP, inventory, etc*)

let get_hp (p : steammon) = p.curr_hp

let change_hp_by (p : steammmon) (delta : int) : steammon = {
	species = p.species; curr_hp = p.curr_hp + delta; max_hp = p.max_hp;
	first_type = p.first_type; second_type = p.second_type;
	first_attack = p.first_attack; second_attack = p.second_attack;
	third_attack = p.third_attack; fourth_attack = p.fourth_attack;
	attack = p.attack; spl_attack = p.spl_attack; defense = p.defense;
	spl_defense = p.spl_defense; speed = p.speed; status = p.status;
	mods = p.mods
}
	
let get_pp (a : attack) = a.pp_remaining
	
let change_pp_by (a : attack) (delta : int) : attack = {
	name = a.name; element = a.element; max_pp = a.max_pp;
	pp_remaining = a.pp_remaining + delta; power = a.power; accuracy = a.accuracy;
	crit_chance = a.crit_chance; effect = a.effect
}

(*i = 1 changes attack, 2 speed, 3 defense, 4 accuracy*)
let change_mods_by (p : steammon ref) (delta : int) (i : int) : unit =
  let change_mods (m : modifier) (delta : int) (i : int): modifier = 
	  match i with
	  | 1 -> 
  		{attack_mod = m.attack_mod + delta;
  		 speed_mod = m.speed_mod;
  		 defense_mod = m.defense_mod;
  		 accuracy_mod = m.accuracy_mod;	
  		} 
	  | 2 ->
  		{attack_mod = m.attack_mod;
  		 speed_mod = m.speed_mod + delta;
  		 defense_mod = m.defense_mod;
  		 accuracy_mod = m.accuracy_mod;	
  		} 
	  | 3 ->
  		{attack_mod = m.attack_mod;
  		 speed_mod = m.speed_mod;
  		 defense_mod = m.defense_mod + delta;
  		 accuracy_mod = m.accuracy_mod;	
  		} 
	  | 4 ->
  		{attack_mod = m.attack_mod;
  		 speed_mod = m.speed_mod;
  		 defense_mod = m.defense_mod;
  		 accuracy_mod = m.accuracy_mod + delta;	
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
		 status = [];
		 mods = change_mods (!p).mods delta i;
		}	 	

let rec all_are_dead (sl : steammon list) : bool =
	match sl with
		| [] -> true
		| h::t -> h.curr_hp = 0 && all_are_dead t