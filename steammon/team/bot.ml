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





























