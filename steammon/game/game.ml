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
	let update_team cmd (old : team) : team =
		match cmd with
			| Action(act) -> (
				match act with
						| PickSteammon(str) -> ()
						| PickInventory(str) -> ()
						| SelectStarter(str)
						| SwitchSteammon(str) ->
							  let newlst = swap_steammon (deref_list old.steammons) str in
								{id = old.id; steammons = reref_list newlst;
								items = id.items}
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
						| UseAttack(str) -> ()
				)
			| _ -> old
	in
	let r_new = update_team ra r_old and b_new = update_team ba b_old in
	(*None, team_data * team_data , Some cmd1, Some cmd2*)
	failwith "implement me!"

let init_game () =
	failwith "implement me!"