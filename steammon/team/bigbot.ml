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

let handle_request c r = 
	match r with
		| StarterRequest(gs) -> 
				