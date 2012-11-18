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

let rec all_are_dead (sl : steammon list) : bool =
	match sl with
		| [] -> true
		| h::t -> h.curr_hp = 0 && all_are_dead t