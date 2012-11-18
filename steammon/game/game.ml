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
	((List.map (fun s -> !s) redTeam.steammons, List.map (fun x -> !x) redTeam.items),
	(List.map (fun s -> !s) blueTeam.steammons, List.map (fun x -> !x) blueTeam.items))

let game_from_data game_data : game = 
	let ((r_slist, r_inv), (b_slist, b_inv)) = game_data in
	let red_team = {id = Red; steammons = List.map (fun s -> ref s) r_slist;
	    items = List.map (fun x -> ref x) r_inv} in
	let blue_team = {id = Blue; steammons = List.map (fun s -> ref s) b_slist;
	    items = List.map (fun x -> ref x) b_inv} in
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