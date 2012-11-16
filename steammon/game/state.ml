open Definitions
open Util
open Attack
(*This State module will be used to keep track of the state of the game*)
(*This includes using items, attacking, switching pokemon, keeping track*)
(*of properties like HP, PP, inventory, etc*)

let get_hp (p : steammon) = p.curr_hp
	
let get_pp (a : attack) = a.pp_remaining
	
let change_pp_by (a : attack) (delta : int) : attack = {
	name = a.name; element = a.element; max_pp = a.max_pp;
	pp_remaining = a.pp_remaining + delta; power = a.power; accuracy = a.accuracy;
	crit_chance = a.crit_chance; effect = a.effect
}