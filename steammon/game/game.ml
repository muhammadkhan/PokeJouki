open Definitions
open Util
open Constants
open Netgraphics

type team = {
	id: color;
	steammons: steammon ref list;
	items: item list
}

(* You have to implement this. Change it from int to yout own state type*)

(*first team = red, second team = blue*)
type game = team * team

let game_datafication g : game_status_data =
	let (redTeam, blueTeam) = g in
	let item_to_int i =
		match i with
			| Ether -> 1
			| MaxPotion -> 2
			| Revive -> 3
			| FullHeal -> 4
			| XAttack -> 5
			| XDefense -> 6
			| XSpeed -> 7
			| XAccuracy -> 8
	in
	((List.map (fun s -> !s) redTeam.steammons, List.map item_to_int redTeam.items),
	(List.map (fun s -> !s) blueTeam.steammons, List.map item_to_int blueTeam.items))

let game_from_data game_data : game = 
	let int_to_item n =
		match n with
			| 1 -> Ether
			| 2 -> MaxPotion
			| 3 -> Revive
			| 4 -> FullHeal
			| 5 -> XAttack
			| 6 -> XDefense
			| 7 -> XSpeed
			| 8 -> XAccuracy
			| _ -> failwith "no item corresponds to this index"
	in
	let ((r_slist, r_inv), (b_slist, b_inv)) = game_data in
	let red_team = {id = Red; steammons = List.map (fun s -> ref s) r_slist;
	    items = List.map int_to_item r_inv} in
	let blue_team = {id = Blue; steammons = List.map (fun s -> ref s) b_slist;
	    items = List.map int_to_item b_inv} in
	(red_team, blue_team)

let handle_step g ra ba : game_output =
	let (r_old, b_old) = g in
	let update_team cmd old =
		match cmd with
			| Action(act) -> (
				match act with
						| SelectStarter(str) -> ()
						| PickSteammon(str) -> () 
						| PickInventory(str) -> ()
						| SwitchSteammon(str) -> ()
						| UseItem(itm, str) -> ()
						| UseAttack(str) -> ()
				)
			| _ -> old
	in
	let r_new = update_team ra r_old and b_new = update_team ba b_old in
	(*None, team_data * team_data , Some cmd1, Some cmd2*)
	failwith "implement me!"

let init_game () =
	failwith "implement me!"