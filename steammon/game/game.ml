open Definitions
open Util
open Constants
open Netgraphics

type team = {
	id: color;
	steammons: steammon ref list;
	items: int ref list (*mutable inventory*)
}

(* You have to implement this. Change it from int to yout own state type*)

(*first team = red, second team = blue*)
type game = team * team

let pool = ref []

let game_datafication g : game_status_data =
	let (redTeam, blueTeam) = g in
	((deref_list redTeam.steammons, deref_list redTeam.items),
	(deref_list blueTeam.steammons, deref_list blueTeam.items))

let game_from_data game_data : game = 
	let ((r_slist, r_inv), (b_slist, b_inv)) = game_data in
	let red_team = {id = Red; steammons = reref_list r_slist;
	    items = reref_list r_inv} in
	let blue_team = {id = Blue; steammons = reref_list b_slist;
	    items = reref_list b_inv} in
	(red_team, blue_team)

let handle_step g ra ba : game_output =
	let (r_old, b_old) = g in
	let update_team cmd (old : team) (old2 : team ref) : team =
		match cmd with
			| Action(act) -> (
				match act with
						| PickSteammon(str) ->
							  let newlst = swap_steammon (!pool) str in
								{id = old.id; steammons = reref_list newlst;
								items = old.items}
						| PickInventory(inv) -> {
							  id = old.id; steammons = old.steammons;
								items = reref_list (List.map2 (+) inv (deref_list old.items))
							}
						| SelectStarter(str)
						| SwitchSteammon(str) ->
							  let newlst = swap_steammon (deref_list old.steammons) str in
								{id = old.id; steammons = reref_list newlst;
								items = old.items}
						| UseItem(itm, str) -> (
							  let sref=ref(steammon_of_string(deref_list old.steammons)str)in
								match itm with
									| Ether -> Item.use_Ether sref; old
									| MaxPotion -> Item.use_maxPotion sref; old
									| Revive -> Item.use_Revive sref; old
                  | FullHeal -> Item.use_FullHeal sref; old
                  | XAttack
                  | XDefense
                  | XSpeed
                  | XAccuracy -> Item.use_X_item itm sref; old
							  )
						| UseAttack(str) ->
							  let battlemon_ref : streammon ref = List.hd old.steammons in
								let battlemon : steammon = !battlemon_ref in
								let atk1 = battlemon.first_attack in
								let atk2 = battlemon.second_attack in
								let atk3 = battlemon.third_attack in
								let atk4 = battlemon.fourth_attack in
								let atk : attack =
									if atk1.name = str then atk1
									else if atk2.name = str then atk2
									else if atk3.name = str then atk3
									else if atk4.name = str then atk3
									else failwith "not a valid attack"
								in
								let dfdr = List.hd (!old2.steammons) in
								let dmg = Attack.normal_attack battlemon atk !dfdr in
								dfdr := State.change_hp_by !dfdr dmg;
								old
				)
			| _ -> old
	in
	let r_new = update_team ra r_old
	and b_new = update_team ba b_old in
	let won =
		if State.all_are_dead (deref_list (r_new.steammons))
		   && State.all_are_dead (deref_list (b_new.steammons)) then
			Some Tie
		else if State.all_are_dead (deref_list (r_new.steammons)) then
			Some (Winner Red)
		else if State.all_are_dead (deref_list (b_new.steammons)) then
			Some (Winner Blue)
		else None
	in
	let r_data = (deref_list r_new.steammons, deref_list r_new.items) in
	let b_data = (deref_list b_new.steammons, deref_list b_new.items) in
	(won, (r_data, b_data) , None (*Some cmd1*), None(*Some cmd2*))

let init_game () =
	failwith "implement me!"