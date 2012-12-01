open Team
open Definitions
open Constants
open Util
open Pick

(** This is our final bot module. 
We only must rewrite our handle request function
1. If we are to start, we must pick a good pokemon. 
2. Thereafter we pick each pokemon accordingly
4. Picking inventory items are also clutch
3. Every action after that will need to be handled as well.

The strategy is explained within this link:
http://kotaku.com/5920285/
five-tips-for-forming-the-perfect-pokemon-team-
from-the-greatest-player-in-the-world
*)

let _ = Random.self_init ()

(*-----------Functions to initiate a switch steammon  -------------*)				

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
	  | _ -> failwith "no steammon left to choose!!"*)  


let handle_request c r = 
	match r with 
		| PickRequest(_,gs, _, sp) ->
			(match sp with
				| h::t -> 
      			let my_pick =  Pick.pick_stmn gs c sp in 
      			print_endline ("picking" ^ my_pick.species);
      			PickSteammon(my_pick.species)
				| [] -> failwith "no steammon to pick!")
		| _ -> failwith "penis" 		

