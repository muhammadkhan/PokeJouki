open Definitions
open State
open Constants

(**
This is the module that handles status effects
but not effects such as self-attacks

*)

(*If the steammon is frozen, it cannot attack that turn*)
let frozen_effect (p : steammon ref) : unit = 
	if List.mem Frozen !p.status then
  	let probability = Random.int 100 in
  	if probability < cDEFROST_CHANCE then
  		(p := change_status_list(!p)(List.filter(fun x-> x <> Frozen)(!p).status);
			Netgraphics.add_update(SetStatusEffects(!p.species,!p.status));
			Netgraphics.add_update(Message(!p.species ^ " unfroze!")))
  	else ()
	else failwith "you can't unfreeze a not frozen steammon"

(*Poison damage that occurs at the end of every turn if the pokemon is poisoned*)
let poison_damage (p : steammon ref) : int = 
	let delta = int_of_float ((cPOISON_DAMAGE *. (float_of_int (!p).max_hp))) in
	p := change_hp_by (!p) (-delta);
	delta
		
(*If the pokemon is sleeping, then we will potentially wake it up*)	
(*It should be able to attack after its waking up though  *)					
let wakeup (p : steammon ref) : unit = 
	let probability = Random.int 100 in 
	 if (probability < cWAKE_UP_CHANCE) then
		(p := change_status_list(!p)(List.filter(fun x->x <> Asleep)(!p).status);
	  Netgraphics.add_update(SetStatusEffects(!p.species,!p.status));
		Netgraphics.add_update(Message(!p.species ^ " woke up!"))
		)
	else ()
		
(*This is the one-time slowdown that happens due to paralysis*)
(*Note : make sure that paralysis is already in the status list*)						
let paralyzed_slowdown (p : steammon ref) : unit = 
	p := change_speed (!p) ((!p).speed / cPARALYSIS_SLOW)
				
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

(*you can only unparalyze it with a full heal*)
let unparalyze (p : steammon ref) : unit =
	if List.mem Paralyzed (!p.status) then
		p := change_speed (!p) (!p.speed * cPARALYSIS_SLOW)
	else
		()