open Team
open Definitions
open Constants
open Util

(**This is the module which our bot will utilize to 
pick pokemon smartly. 
*)

let types = [Fire; Water; Ice
  ; Grass; Poison; Normal; Flying; Psychic; Ghost; Dark; Steel
  ; Rock; Ground; Electric; Bug; Dragon; Fighting]

let get_atk_lst (s : steammon) = 
		[s.first_attack; s.second_attack; s.third_attack; s.fourth_attack]


(**First we design all of the helper 
functions pertaining to picking pokemon*)
let oppo_pkmn = ref [] 
let pick_num = ref 0 

(*returns the pokemon that were chosen since we last saw the steam_pool *)
let find_missing (o: steam_pool) (n: steam_pool) : steammon list = 
	let find acc p = 
		if List.mem p n then acc
		else p::acc
	in
	List.fold_left find [] o

	
(*This is the same function but making use of the accessible game_data*)
(*o is the list of pokemon we know the opponent has*)
let find_newbies (o : steam_pool) (n : game_status_data) (c:color) : steam_pool = 
	let (op_team_o,op_team_n) =
		(*We are picking the team here *) 
		match c with
		| Red -> (o, (fst (snd n)))
		| Blue -> (o , (fst (fst n)))   		
	in
  let find acc p =
		if List.mem p op_team_n then acc
		else p::acc				
	in
	List.fold_left find [] op_team_o													
												
(****___________Everything after this is valid_______________ *****)								

(*Still valid*)
(*finds the types that the pokemon is weak to*)	
let find_weakness (s:steammon) : steamtype list = 
	let f acc (t: steamtype) =
		match s.second_type with
			| None -> 
		  		(if (weakness t (valOf s.first_type) > 1.) then t::acc
					 else acc)
			| Some x -> 
				(if (weakness t (valOf s.first_type) > 1.) || (weakness t x > 1.) then t::acc
				 else acc)		 	
	in
	List.fold_left f [] types

(**This narrows down to the 
types of pokemon that we know our opponents pokemon are weak to*)
let narrow_pick_type (i : int ref) (sp :steam_pool) : steamtype list = 
  if !i = 0 then 
		(oppo_pkmn := sp;
		incr(i);
		types)
  else 
    (let op_picks = find_missing (!oppo_pkmn) sp in
		 (*This is the function to figure out the opponents weaknesses*)
		 let f acc (s:steammon) =
			 (find_weakness s) @ acc
		 in
		 incr(i);
		 oppo_pkmn := sp;
		 List.fold_left f [] op_picks)  

(*This is going to be valid*)
(*outputs a list of pokemon which adhere to the types picked *)
let pick_types (t_lst : steamtype list) (sp: steam_pool) = 
  let filter_help (x : steammon) = 
		(List.mem (valOf x.first_type)	t_lst) || 
		(if x.second_type = None then false 
		 else List.mem (valOf x.second_type) t_lst) 
  in
	List.filter filter_help sp

(*returns the effective attack we should use to consider points*)
let atk_eff (p : steammon) : int =
  (*returns true if at least two attacks are special*)
	let has_special_majority (p : steammon) : bool =
  	let special_list = [Electric; Fire; Water; Psychic; Ghost] in
    let count (r : int) (elem : attack) =
  		if List.mem elem.element special_list then r + 1
  		else r
  	in
  	let ct = List.fold_left count 0 (get_atk_lst p) in
    ct >= 2
  in
	if has_special_majority p then p.spl_attack
	else p.attack

(*Still valid*)
(*returns the points, a measure of value, of the given steammon*)
let compute_points (ps : steammon list) : (steammon * float) list =
	(*stab bonus : +6 per attack that actually gives a stab bonus*)
	(* poisons : +3 * accuracy * effect chance / 10000*)
	(*confuses : + 2 ...........*)
	(*sleep : +5...............*)
	(*paralyzes : +2...............*)
	(*freezes : +3.................*)
	(*add hp / 100*)
	(*add (effective_attack + speed) / 50*)
	(*add defense / 100*)
	let points_of (p : steammon) =
		let stab_bonus =
			let f r elem =
				if (elem.element = (valOf p.first_type)) ||
				   (if p.second_type = None then false
					  else (elem.element = (valOf p.second_type))) then
					r + 6
				else r
			in
			List.fold_left f 0 (get_atk_lst p)
		in
		let eff_pts =
			let f r att =
				let (eff, chance) = att.effect in
				match eff with
					| Poisons -> r +. (float_of_int(1*att.accuracy*chance)) /. 10000.
					| Confuses -> r +. (float_of_int(0*att.accuracy*chance)) /. 10000.
					| Sleeps -> r +. (float_of_int(4*att.accuracy*chance)) /. 10000.
					| Paralyzes -> r +. (float_of_int(2*att.accuracy*chance)) /. 10000.
					| Freezes -> r +. (float_of_int(3*att.accuracy*chance)) /. 10000.
					| _ -> r
			in
			List.fold_left f 0. (get_atk_lst p)
		in
		let hp_pts = ((float_of_int p.max_hp) /. 2.) /. 100. in
		let as_pts = (float_of_int((atk_eff p) + p.speed)) /. 100. in
		let d_pts = (float_of_int p.defense) /. 50. in
		( p, (float_of_int stab_bonus)+. (eff_pts /. 10.)  +. hp_pts +. as_pts +. d_pts )
	in
	List.map points_of ps

(*Still valid*)
(*picks the best steammon statistically*)
let most_suitable_steammon (lst : (steammon*float) list) : steammon =
	let choose ((acc : steammon list),(curMax : float)) (data : steammon * float) =
		let (s, f) = data in
		if f > curMax then ([s],f)
		else if f = curMax then (s::acc, f)
		else (acc, curMax)
	in
	match (fst (List.fold_left choose ([],min_float) lst)) with
		| h::_ -> h
		| [] -> failwith "this has no head"

(*This is the final method, where we pick the pokemon we want to select next*)
let pick_stmn (gs: game_status_data) (c:color) (sp:steam_pool) : steammon = 
  let added_pkmn = find_newbies !oppo_pkmn gs c in 
	 (*All of the types that are super effective*)
    let effective_types = narrow_pick_type pick_num added_pkmn in 
		let valid_pkmn = pick_types effective_types sp in 
		most_suitable_steammon (compute_points valid_pkmn)
		
(*------ This section is geared towards a different style of pick ------*)

				
(*The points system here has been reworked, and it basically picks the highest hp*)												
let smart_pick (gs:game_status_data) (c : color) (sp : steam_pool) : steammon = 
	(*The points do not have much type variation*)
	let (r,b) = gs in
	let (mons, pack) = if c = Red then r else b in 
	let points = compute_points sp in
	let f (x : steammon * float) = 
		let y = (fst(x)).first_type in 
		if List.exists (fun z -> z.first_type = y) mons then false
		else true 
	in 
	let re_points = List.filter f points in 
	most_suitable_steammon re_points	

						
(*This is our final picking method, which results in a smarter pick.*)																		
let revised_pick  (gs : game_status_data) (c : color) (sp : steam_pool) : steammon = 
	let (r , b) = gs in 
	let (op_mons, _) = if c = Red then b else r in 
	  match op_mons with
			| [] -> smart_pick gs c sp
			| h::_ -> 
				(let t_weak = find_weakness h in 
				 let n_pool = List.filter (fun x -> List.mem (valOf(x.first_type)) t_weak) sp in
				 try smart_pick gs c n_pool with _ -> smart_pick gs c sp)

(*------- Picking the items is hardcoded in---------*)
																																																																																																																																																																																																																													
(*Pick Items is fine*)										
let pick_items : int list = 
	let y = Constants.cINITIAL_CASH / 2 in 
	let (a,b) = ((y / cCOST_MAXPOTION), (y / cCOST_REVIVE)) in 
	[0; a; b; 0; 0; 0; 0; 0]												
														
(*---------- The section for handling an action request  ----------*)

(*we are normally picking high_hp steammon, so this is good.*)
let use_potion (s : steammon) : bool = 
	let x = s.max_hp / 3 in
	s.curr_hp <= x 

(*------we are going for the most powerful attacks-------*)

(**The only things we are worrying about here are:
1. super-effectiveness
2. STAB bonus
3. initial damage*)
let score_attack (a : attack) (at :steammon) (df : steammon) : float =
	if a.power < 20 then 1.0
	else 
    let x =
  		 weakness (valOf(df.first_type)) a.element in
  	let y = try weakness (valOf(df.second_type)) a.element with _ -> 1.0 in	
  	let z = 
			if (valOf (at.first_type)) = a.element 
			  then cSTAB_BONUS 
			else if 
			   (try (valOf(at.second_type)) = a.element with _ -> false) then cSTAB_BONUS
			else 1.0 in	
  	(float_of_int(a.power)) *. x *. y *. z

(*This picks the most powerful attack to use*)
(*The idea is that we want to damage them every single turn*)
(*This will output the name of the attack that we want *)
let power_attack (gs: game_status_data) (s:steammon) (c : color) : string = 
	let (r, b) = gs in
	let my_team = if c = Red then r else b in
	let op_team = if c = Red then b else r in  
	let (mons, _) = my_team in
	let (op_mons, _) = op_team in  
	  match mons with
		|h::_ ->
			(
			(*We now have all the attacks of the pokemon*)
			(*We should filter the ones that have no pp left*)
			let atks = List.filter (fun x -> x.pp_remaining > 0) (get_atk_lst h) in 
			let final_atk = ref "" in
			let score = ref 0. in  
			(*using the score function above, we will fold over the list *)  
			let f_atk (a : attack)  =																																
				if (score_attack a h (List.hd op_mons)) > !score  then
				 (score := score_attack a h (List.hd op_mons);
					final_atk := a.name;)
				else ()																																																										
			in
			List.iter f_atk atks;
			!final_atk)
		| _ -> failwith "This cannot happen"

(*Picks the attack that will apply a status to the victim*)

(*let status_attack (gs : game_status_data) (c : color) : string = 
	let (r,b) = gs in 
	let my_team = if c = Red then r else b in
	let (mons, _) = my_team in 
	match mons with 
		| h::_ -> 
			(let atks = List.filter (fun x -> x.pp_remaining > 0) (get_atk_lst h) in 
			 let f (a : attack) = 
				 let (eff,_) = a.effect in
				 match eff with
					| Poisons|Confuses|Sleeps|Paralyzes|Freezes -> true
					| _ -> false
			 in 
			 let atks' = List.filter f atks in 
			   	   
			)


    | _ -> failwith "Not possible"

*)

																																									
(*----- Functions to initiate a switch/pick a starter steammon  -----*)	

(**Amongst our own team we are picking the next steammon
 such that we are able to have a type advantage*)
let switch_for_advantage (gs: game_status_data) (c:color) : steammon =
  let (r, b) = gs in 
	  let my_team = if c = Red then r else b in 
	    let (mons,_) = my_team in 
  revised_pick gs c mons				
									
																											
(*(*We will only switch for advantage*)
let switch_for_advantage (gs : game_status_data) (c: color) : steammon = 
	let (r,b) = gs in 
	(*We find the opposing team*)
	let op_team = if c  = Red then b else r in 
	  let (mons , _ ) = op_team in 
		match mons with
		| h::_ -> 
			(*We find the weaknesses to the current pokemon*)
			let weak_to = find_weakness h in
			let y = List.filter (fun x -> (List.mem x.first_type weak_to)) (fst(r)) in
			if y <> [] then List.hd y else failwith "no advantages left!"
			
	  | _ -> failwith "no steammon left to choose!!" *) 																						
																								
																										
																												
																														
																																
																																		
																																				
																																						
																																								
																																										
																																												
																																														
																																																
																																																		
																																																				
																																																						    
