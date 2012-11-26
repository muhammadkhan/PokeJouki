open Team
open Definitions
open Constants

(** This is our final bot module. 
We only must rewrite our handle request function
1. If we are to start, we must pick a good pokemon. 
2. Thereafter we pick each pokemon accordingly
4. Picking inventory items are also clutch
3. Every action after that will need to be handled as well.

The strategy (plainly) is explained within this link:
http://kotaku.com/5920285/five-tips-for-forming-the-perfect-pokemon-team-from-the-greatest-player-in-the-world
*)

let _ = Random.self_init ()
let types = [Fire;Water;Ice;Grass;Poison;
  Normal;Flying;Psychic;Ghost;
  Dark;Steel;Rock;Ground;Electric;
  Bug;Dragon;Fighting]

let get_atk_lst (s : steammon) = 
	let atk_lst = 
		[s.first_attack; s.second_attack; s.third_attack; s.fourth_attack] in
  atk_lst

(*This will determine how versatile the steammon's attacks are.*)
(*This looks mainly for type matching, not versatility*)
(*Highest score is 2 ^ 3*)
let det_atk_value (s:steammon) : int = 
	(*If the attacks match with any of the pokemon's types it gets a bonus *)
	let (t1,t2) = (s.first_type, s.second_type) in
	let score = ref 1 in 
	let atks = get_atk_lst s in 
	(*This are the attacks that cause damage*)	 
	let damaging = 
		List.filter (fun (x : attack) -> x.power <> 0) atk_lst in
	List.iter (fun x -> if (Some (x.element) = t1 || Some (x.element) = t2) then
		score := !score * 2 else score := !score + 1;) damaging;
	!score	  			

(*Values the effects that we want for our team.*)
(*Note: we could include another parameter, where we can search for a particular effect*)
let det_effect_value (s : steammon) : int = 
	let score = ref 1 in 
	let atks = get_atk_lst s in 
	let effects =
		let ef_help x = 
			match x with
			| (Nada, _) -> 0
			| (_,_) -> 1 in
		List.filter ef_help atks in
	(*This may change based on our valuation of the effects, we are basing our strat on*)	
	let f (e : attack) = 
		match e.effect with
			| (Poisons, _) -> score := !score + 3;
			| (Confuses, _) -> score := !score + 2;
			| (Sleeps, _) -> score := !score + 5;
			| (Paralyzes, _) -> score := !score + 2;
			| (Freezes, _) -> score := !score + 3;
			| (_,_) -> ()	 	 		 	 	   
	 in 
   List.iter f effects;
	 !score
	
(**Muhammad's idea for this is to store a queue of the least common weaknesses
Much like keeping a pool of shared weaknesses, and how many pokemon in the pool
have this particular weakness. a pool of tuples of steamtype*int*)		
	
(*This determines if this pokemon shares weaknesses with the rest of your team*)		
let det_sim_weakness (s:steammon) (a : team_data): bool =
	(*The steammon-list that is our team*) 
	let (my_team,_) = a in 
	(*we will fold over the list and use types as the accumulator*) 
	let f (a : steamtype list)  (b : steammon): steamtype list = 
		match (b.first_type, b.second_type) with
		| (Some x, Some y) ->
			List.filter (fun z -> ((weakness z x) <= 1.0) && ((weakness z y) <= 1.0)) a  		
	  | (Some x, _) -> 
			List.filter (fun z -> (weakness z x) <= 1.0) a
		| (_,_) -> failwith "this is not a possible type for a steammon"
	in
  let common_weak = List.fold_left f types my_team in
	  if ((f common_weak s) <> []) then false
	  else true  	
	

(**Determine how vulnerable this pokemon is to the other team.
And how similar it is to pokemon on your own team
and if it shares any weaknesses with any of your current pokemon
We will assume that you are team a, and team b is your opponent*)


(*let det_pkm_pick (s :steammon) (a : team_data) (b : team_data) = 
	(*The respective steammon in each team*)
	let (my_team,_) = a in
	let (op_team,_) = b in *)
	 	






				