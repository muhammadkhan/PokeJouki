open Team
open Definitions
open Constants
open Util
open Pick

(** This is our final bot module. 
We only must rewrite our handle request function
1. If we are to start, we must pick a good pokemon. 
2. Thereafter we pick each pokemon accordingly
3. Every action after that will need to be handled as well.

The strategy is explained within this link:
http://kotaku.com/5920285/
five-tips-for-forming-the-perfect-pokemon-team-
from-the-greatest-player-in-the-world
*)
let potion = ref true

let handle_request c r = 
	match r with
		| StarterRequest(gs) -> 
			let s = switch_for_advantage gs c in
			potion := true;
			SelectStarter(s.species) 
		| PickRequest(_,gs, _, sp) ->
			(match sp with
				| h::t ->
					  (*THe picks should go better, this is not as good as it should be*) 
      			let my_pick =  Pick.revised_pick gs c sp in 
      			print_endline ("picking" ^ my_pick.species);
      			PickSteammon(my_pick.species)
				| [] -> failwith "no steammon to pick!")
		| PickInventoryRequest(gr) ->
			  PickInventory(Pick.pick_items)
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
          						   if (use_potion h && (x >= 2.0)) then
          								let s = switch_for_advantage gr c in 
          								  SwitchSteammon(s.species)								
          							 else 	
            							 let s = power_attack gr h c in
            							   UseAttack(s))))
					         | _ -> failwith "hello")
				| _ -> failwith "nope"


let () = run_bot handle_request