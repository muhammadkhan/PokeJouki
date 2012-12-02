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
let potion = ref false

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
			let my_team = if c = Red then r else b in
			let (mons, _)  = my_team in
			  match mons with
					| h::_ ->
						(if (use_potion h) && not (!potion) then UseItem(MaxPotion, h.species)
						 else
							 let s = power_attack gr h c in
							 (*if s.power = 0 then
								let y = List.filter (fun x -> x <> s && x.pp_remaining > 0) (get_atk_lst h) in
								UseAttack((List.hd y).name)
							 else*) 
							   UseAttack(s))
					| _ -> failwith "Should not happen"			

let () = run_bot handle_request

