open Definitions
open Util
open Constants

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

(*This applies the effect of an attack to the steammon, only if the attack hits *)
let apply_effect (a : attack ref) (df : steammon ref) : unit =
	let (eff,chance) = !a.effect in 
	let rand = Random.int 100 in
	if rand < chance then 
		let f (p : steammon) (e: status) =
			let lst = p.status in 
			List.mem e lst
		in
		let add_eff (s : status) =
			if(not(f !df s) && (!df.status = [] || !df.status = [Confused])) then
				df := State.change_status_list !df (s::(!df.status))
			else ()
		in
	  match eff with
			| Nada -> ()
			| Confuses ->
				(if !df.status = [] then
				  (df := State.change_status_list !df (Confused::(!df.status));
					Netgraphics.add_update(SetStatusEffects(!df.species, !df.status));
					Netgraphics.add_update(Message(!df.species ^ " IS CONFUSED!!!!!!!!!!")))
			  else () )
			| Poisons ->
				add_eff Poisoned;
				Netgraphics.add_update(SetStatusEffects(!df.species, !df.status));
				Netgraphics.add_update(Message(!df.species ^ " IS POISONED!!!!!!!!!!"))
			| Sleeps ->
				add_eff Asleep;
				Netgraphics.add_update(SetStatusEffects(!df.species, !df.status));
				Netgraphics.add_update(Message(!df.species ^ " IS ASLEEP!!!!!!!!!!"))
			| Paralyzes ->
				Status.paralyzed_slowdown df;
				add_eff Paralyzed;
				Netgraphics.add_update(SetStatusEffects(!df.species, !df.status));
				Netgraphics.add_update(Message(!df.species ^ " IS PARALYZED!!!!!!!!!!"))
			| Freezes ->
				add_eff Frozen;
				Netgraphics.add_update(SetStatusEffects(!df.species, !df.status));
				Netgraphics.add_update(Message(!df.species ^ " IS ZARDOZLY FROZEN!!!!!!!!!!"))
			| _ -> () (*TODO: change*)
	else ()

(*should this return float and return the damage*)
(*or should it return unit and simply update the values accordingly*)
(*We can update this for steammon refs too*)
let normal_attack (at : steammon) (a : attack ref) (df: steammon) : float =
	let pow = float_of_int !a.power in
	let (attackersattack, opponentsdef) =
		match !a.element with
			| Fire
			| Water
			| Psychic
			| Ghost -> (float_of_int at.spl_attack, float_of_int df.spl_defense)
			| _ -> (float_of_int at.attack, float_of_int df.defense)
	in
	let crit_effect = crit_hit_mult !a in
	let stab_bonus =
		if Some !a.element = at.first_type || Some !a.element = at.second_type then
			cSTAB_BONUS
		else 1.
	in
	let st_mult = find_atk_mult !a df in
	let happens = Random.int 100 in
	if happens < !a.accuracy then
		(a := State.change_pp_by (!a) (-1);
	  (pow*.attackersattack*.crit_effect*.stab_bonus*.st_mult)/.opponentsdef)
	else 0.
			
(**This is the attack damage that happens 
if a confused pokemon attacks itself	
The attack is typeless, 
and determined between the pokemons own attack and defense*)
let confused_attack (p : steammon) : float = 
	float_of_int ((cSELF_ATTACK_POWER * p.attack) / p.defense) 

	
(*This is the attack sequence that the steammon pulls off if it is confused*)
(*Only done if the attacker is confused*)			
let atk_while_confused (a : attack ref) (at : steammon ref) (df : steammon ref): float = 
	if Status.snap_out_confusion then
		(at := State.change_status_list !at (List.filter (fun x -> x <> Confused) !at.status);
		normal_attack !at a !df)
		  (*df := State.change_hp_by !df (0 - int_of_float(dmg));)  *)
	else
		( if Status.confused_self_attack then
			 let self_dmg = confused_attack !at in
			 at := State.change_hp_by !at (0 - int_of_float(self_dmg)); 
			0.
		  else
			 normal_attack !at a !df
		   (*df := State.change_hp_by !df (0 - int_of_float(dmg));	*)	
		)
		
let atk_while_asleep (a :attack ref) (at : steammon ref) (df : steammon ref) : float = 
	Status.wakeup at;
	if List.mem Asleep (!at.status) then
		0.
	else
		if List.mem Confused !at.status then
			atk_while_confused a at df
		else
			normal_attack (!at) a (!df)
		(*df := State.change_hp_by !df (0 - int_of_float(dmg))*)

let atk_while_para (a : attack ref) (at : steammon ref) (df : steammon ref) : float = 
	if Status.paralyze_attack then
		if List.mem Confused !at.status then
			atk_while_confused a at df
		else
			normal_attack (!at) a (!df)
		(*df := State.change_hp_by !df (0 - int_of_float(dmg));		*)				
	else 0.

(*Frozen is covered in status, as well as poisoned*)	
(*This is the final attacking sequence.*)
let final_attack (a : attack ref) (at : steammon ref) (df : steammon ref) : float =
	if List.mem Paralyzed !at.status then atk_while_para a at df
	else if List.mem Asleep !at.status then atk_while_asleep a at df
	else if List.mem Frozen !at.status then (Status.frozen_effect at; 0.)
	else if List.mem Confused !at.status then atk_while_confused a at df
	else normal_attack (!at) a (!df)
		 
				
						
								
										
												
														
																
																		
																				
																						
																								
																												
