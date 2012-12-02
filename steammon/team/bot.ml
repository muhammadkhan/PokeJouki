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

let handle_request c r = 
	match r with 
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
		| _ -> failwith "penis" 		

let () = run_bot handle_request

