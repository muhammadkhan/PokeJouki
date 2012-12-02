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
		let hp_pts = ((float_of_int p.max_hp) /. 2.) /. 100. in
		let as_pts = (float_of_int((atk_eff p) + p.speed)) /. 100. in
		let d_pts = (float_of_int p.defense) /. 50. in
		( p, (float_of_int stab_bonus)+. hp_pts +. as_pts +. d_pts )
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
	let (r, b) = gs in 
	let (op_mons, _) = if c = Red then b else r in 
	  match op_mons with
			| [] -> smart_pick gs c sp
			| h::_ -> 
				(let t_weak = find_weakness h in 
				 let n_pool = List.filter (fun x -> List.mem (valOf(x.first_type)) t_weak) sp in
				 try smart_pick gs c n_pool with _ -> smart_pick gs c sp)

(*------- Picking the items is hardcoded in---------*)
																																																																																																																																																																																																																													
let pick_items : int list = 
	let y = Constants.cINITIAL_CASH / 2 in 
	let (a,b) = ((y / cCOST_MAXPOTION), (y / cCOST_REVIVE)) in 
	[0; a; b; 0; 0; 0; 0; 0]												
														
(*---------- The section for handling an action request  ----------*)

(*we are normally picking high_hp steammon, so this is good.*)
let use_potion (s : steammon) : bool = 
	let x = s.max_hp / 2 in
	s.curr_hp <= x 

(*------we are going for the most powerful attacks-------*)

(**The only things we are worrying about here are:
1. super-effectiveness
2. STAB bonus
3. initial damage*)

let score_attack (a : attack) (at :steammon) (df : steammon) : float =
	if a.power = 0 then 0.
	else 
    let x =
  		 weakness a.element (valOf(df.first_type)) in
  	let y = try weakness a.element (valOf(df.second_type)) with _ -> 1.0 in	
  	let z = 
			if (valOf (at.first_type)) = a.element 
			  then cSTAB_BONUS 
			else if 
			   (try (valOf(at.second_type)) = a.element with _ -> false) then cSTAB_BONUS
			else 1.0 in
		if x *. y >= 2.0 then 10000.
		else
			(float_of_int(a.power)) +. x +. y +. z

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
			let atks = (*List.filter (fun x -> x.pp_remaining > 0)*) (get_atk_lst h) in
			(*match List.filter (fun x -> x.name = "Psychic") atks with
				| [x] -> x.name
				| _ ->  *)
    			let final_atk = ref (s.second_attack).name in
    			let score = ref 0. in  
    			(*using the score function above, we will fold over the list *)  
    			let f_atk (a : attack)  =																																
    				if (score_attack a h (List.hd op_mons)) > !score && (a.pp_remaining > 0)  then
    				 (score := score_attack a h (List.hd op_mons);
    					final_atk := a.name;)
    				else ()																																																										
    			in
    			List.iter f_atk atks;
    			!final_atk)
    | _ -> failwith "This cannot happen"

																																									
(*----- Functions to initiate a switch/pick a starter steammon  -----*)	

(**Amongst our own team we are picking the next steammon
 such that we are able to have a type advantage*)
(*REWORK!!!!!!*)
let switch_for_advantage (gs: game_status_data) (c:color) : steammon =
  let (r, b) = gs in 
	  let (my_team,op_team) = if c = Red then (r,b) else (b,r) in 
	    let (mons,_) = my_team and (op_mons,_) = op_team in
			let lst = List.filter (fun x -> x.curr_hp > 0) mons in
			(* the weaknesses of the pokemon*)
			let t_weak = find_weakness (List.hd(op_mons)) in
			let f x = 
				let f_t = valOf(x.first_type) in
				match x.second_type with
					| None -> 
						List.mem f_t t_weak
					| Some z ->
						(List.mem f_t t_weak)	|| (List.mem z t_weak)
				in
				let lst' = List.filter f lst in
				try List.hd lst' with _ -> List.hd lst 		 
			
			(*Want to make sure that we are not just picking the head every time*)
     (* try revised_pick gs c lst with _ -> List.hd lst*) 				
																															


(** This is our final bot module. 
We only must rewrite our handle request function
1. If we are to start, we must pick a good pokemon. 
2. Thereafter we pick each pokemon accordingly
3. Every action after that will need to be handled as well.
*)
let potion = ref true
let turns_out = ref 0

let handle_request c r = 
	match r with
		| StarterRequest(gs) -> 
			let s = switch_for_advantage gs c in
			turns_out := 0;
			potion := true;
			SelectStarter(s.species) 
		| PickRequest(_,gs, _, sp) ->
			(match sp with
				| h::t ->
					  (*THe picks should go better, this is not as good as it should be*) 
      			let my_pick =  revised_pick gs c sp in 
      			print_endline ("picking" ^ my_pick.species);
      			PickSteammon(my_pick.species)
				| [] -> failwith "no steammon to pick!")
		| PickInventoryRequest(gr) ->
			  PickInventory(pick_items)
		| ActionRequest(gr) ->
			let (r,b) = gr in 
			let (my_team, op_team) = if c = Red then (r,b) else (b,r) in
			  match my_team with
				| (mons, [_;l;m;_;_;_;_;_]) -> 
        			  let (op_mons, _) = op_team in
        			  (match mons with
        					| h::_ ->
        						(let fainted = List.filter (fun x -> x.curr_hp = 0) mons in
        						if (List.length fainted > 2 && (m > 0) ) then 
											UseItem(Revive, (List.hd(fainted)).species)
										else	
          						(if (use_potion h) && (!potion) && (l > 0) then
          							(potion := false;
												turns_out := !turns_out + 1;
          							UseItem(MaxPotion, h.species))
          						 else
          							 (* if we are weak and there is another better choice*)
          							 (let x = 
          								match (List.hd (op_mons)).second_type with
          									| None -> 
          										weakness (valOf((List.hd op_mons).first_type)) (valOf(h.first_type)) 
          									| Some y -> 
          										weakness y (valOf(h.first_type)) +. 
          										  weakness (valOf((List.hd op_mons).first_type)) (valOf(h.first_type)) in 
          						   if (use_potion h && (x >= 2.0)) || (!turns_out > 10) then
          								let s = switch_for_advantage gr c in
													  turns_out := 0; 
          								  SwitchSteammon(s.species)								
          							 else 	
            							 (let s = power_attack gr h c in
													   turns_out := !turns_out + 1;
            							   UseAttack(s)))))
					         | _ -> failwith "hello")
				| _ -> failwith "nope"

let () = run_bot handle_request