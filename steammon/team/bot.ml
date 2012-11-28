open Team
open Definitions
open Constants
open Util

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

let types = [Fire; Water; Ice
  ; Grass; Poison; Normal; Flying
  ; Psychic
  ; Ghost
  ; Dark
  ; Steel
  ; Rock
  ; Ground
  ; Electric
  ; Bug
  ; Dragon
  ; Fighting]

let get_atk_lst (s : steammon) = 
		[s.first_attack; s.second_attack; s.third_attack; s.fourth_attack]


(**First we design all of the helper 
functions pertaining to picking pokemon*)
let old_pool = ref [] 
let pick_num = ref 0 

(*returns the pokemon that were chosen since we last saw the *)
let find_missing (o: steam_pool) (n: steam_pool) : steammon list = 
	let find acc p = 
		if List.mem p n then acc
		else p::acc
	in
	List.fold_left find [] o	

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
		(old_pool := sp;
		incr(i);
		types)
  else 
    (let op_picks = find_missing (!old_pool) sp in
		 (*This is the function to figure out the opponents weaknesses*)
		 let f acc (s:steammon) =
			 (find_weakness s) @ acc
		 in
		 incr(i);
		 old_pool := sp;
		 List.fold_left f [] op_picks)  

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
					| Poisions -> r +. (float_of_int(3*att.accuracy*chance)) /. 10000.
					| Confuses -> r +. (float_of_int(2*att.accuracy*chance)) /. 10000.
					| Sleeps -> r +. (float_of_int(5*att.accuracy*chance)) /. 10000.
					| Paralyzes -> r +. (float_of_int(2*att.accuracy*chance)) /. 10000.
					| Freezes -> r +. (float_of_int(3*att.accuracy*chance)) /. 10000.
					| _ -> r
			in
			List.fold_left f 0. (get_atk_lst p)
		in
		let hp_pts = (float_of_int p.max_hp) /. 100. in
		let as_pts = (float_of_int((atk_eff p) + p.speed)) /. 50. in
		let d_pts = (float_of_int p.defense) /. 100. in
		( p, (float_of_int stab_bonus)+. eff_pts +. hp_pts +. as_pts + d_pts )
	in
	List.map points_of ps





















