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

(**This will determine how versatile the steammon's attacks are.*)
let det_atk_value (s:steammon) = 
	(*If the attacks match with any of the pokemon's types it gets a bonus *)
	let s_type = (s.first_type, s.second_type) in
	let score = ref 0 in 
	let attacks = 
		(s.first_attack , s.second_attack, s.third_attack, s.fourth_attack) in 
		




(**Determine how vulnerable this pokemon is to the other team.
And how similar it is to pokemon on your own team
and if it shares any weaknesses with any of your current pokemon*)
let determine_pkm_pick (s :steammon) = ()	











let handle_request c r = 
	match r with
		| StarterRequest(gs) -> 
				