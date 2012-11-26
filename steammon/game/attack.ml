open Definitions
open Util

(**
This is the module that handles when a pokemon is attacked by a certain type of attack. 
This will evaulate an attack from a pokemon to another pokemon. 
*)

(*The util module has the multiplier values for atk type and defend type*)

(**given an attack on steammon p, 
find the multiplier associated with the attack*)
let find_atk_mult (atk : attack) (p : steammon) : float = 
	let (p_typ1, p_typ2, atk_type) = 
		(p.first_type, p.second_type, atk.element) in 
		 match p_typ1, p_typ2 with
			| Some x , Some y -> 
				(weakness atk_type x) *. (weakness atk_type y)
	    | Some x , None ->
				weakness atk_type x
			| None, _ -> failwith "Steammon must have initial type"	
	
(**given an attack,
 this figures out if it has a crit hit
Then proceeds to give us the multiplier for it*)			
let crit_hit_mult (a : attack) : float = 
	let crit_prob = float_of_int(a.crit_chance) in
	let rand = Random.float 100. in 
	if (rand < (crit_prob *. 100.)) 
	  then cCRIT_MULTIPLIER
	else 1.0	 	 

(*should this return float and return the damage*)
(*or should it return unit and simply update the values accordingly*)
(*We can update this for steammon refs too*)
let normal_attack (at : steammon) (a : attack) (df: steammon) : float =
	let pow = float_of_int a.power in
	let (attackersattack, opponentsdef) =
		match a.element with
			| Fire
			| Water
			| Psychic
			| Ghost -> (float_of_int at.spl_attack, float_of_int df.spl_defense)
			| _ -> (float_of_int at.attack, float_of_int df.defense)
	in
	let crit_effect = crit_hit_mult a in
	let stab_bonus =
		if Some a.element = at.first_type || Some a.element = at.second_type then
			cSTAB_BONUS
		else 1.
	in
	let st_mult = find_atk_mult a df in
	let happens = Random.int 100 in
	if happens < atk.accuracy then
	  (pow*.attackersattack*.crit_effect*.stab_bonus*.st_mult)/.opponentsdef
	else 0.
	
	
(**This is the attack damage that happens 
if a confused pokemon attacks itself	
The attack is typeless, 
and determined between the pokemons own attack and defense*)
let confused_attack (p : steammon) : float = 
<<<<<<< HEAD
	float_of_int ((cSELF_ATTACK_POWER * p.attack) / p.defense)
=======
	float_of_int ((cSELF_ATTACK_POWER * p.attack) / p.defense) 
		
			
>>>>>>> db5acb57d42529a334bb2858f5fb1426d6be84f8
