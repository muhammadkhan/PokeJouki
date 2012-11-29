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

let atks = ref []


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


(*All of these should be able to be done if the pool in initialized properly*)
let handle_step g ra ba : game_output =
	let (r_old, b_old) = g in
	let update_team cmd (old : team) (old2 : team ref) : team =
		match cmd with
			| Action(act) -> (
				(*have an if !pool = [] then ... else ()*)
				match act with
				| PickSteammon(str) -> 
						let (newlst, newpool) = List.partition (fun p -> p.species = str) (!pool) in
						pool := newpool;
						let newmon = 
							(match newlst with
								| [] -> failwith "not valid steammon"
								| h::_ -> h
							)
						in
						Netgraphics.add_update(UpdateSteammon(newmon.species, newmon.curr_hp, newmon.max_hp,old.id));
						{id=old.id; steammons = (ref newmon)::(old.steammons); items = old.items}
						(*| PickInventory(inv) ->
								{id = old.id; steammons = old.steammons;
								items = reref_list (List.map2 (+) inv (deref_list old.items))}
						| SelectStarter(str)
						| SwitchSteammon(str) ->
							  print_endline("enter switch");
							  let newlst = swap_steammon (deref_list old.steammons) str in
								let newlst' = if List.length newlst > 0 then newlst
								              else failwith "vagina"
								in
								print_endline("exit switch");
								Netgraphics.add_update (SetChosenSteammon ((List.hd newlst').species));
								{id = old.id; steammons = reref_list newlst;
								items = old.items}
						| UseItem(itm, str) -> (
							  let sref=ref(steammon_of_string(deref_list old.steammons) str)in
								print_endline("item match case");
									match itm with
									| Ether -> Item.use_Ether sref; old
									| MaxPotion -> Item.use_maxPotion sref; old
									| Revive -> Item.use_Revive sref; old
                  | FullHeal -> Item.use_FullHeal sref; old
                  | XAttack -> Item.use_X_item itm sref; old
                  | XDefense -> Item.use_X_item itm sref; old
                  | XSpeed -> Item.use_X_item itm sref; old
                  | XAccuracy -> Item.use_X_item itm sref; old
							  )
						| UseAttack(str) ->
							  let battlemon_ref : steammon ref = List.hd old.steammons in
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
								let dmg = int_of_float (Attack.normal_attack battlemon (ref atk) !dfdr) in
								dfdr := State.change_hp_by !dfdr dmg;
								let msg =
									if dmg = 0 then "Miss =("
									else "Hit!"
								in
								Netgraphics.add_update (NegativeEffect(msg, !old2.id, dmg));
								old*)
								| _ -> old
				)
			| _ -> old
	in
	let r_new = update_team ra r_old (ref b_old)
	and b_new = update_team ba b_old (ref r_old) in
	let won = 
		(*if State.all_are_dead (deref_list (r_new.steammons))
		   && State.all_are_dead (deref_list (b_new.steammons)) then
			Some Tie
		else if State.all_are_dead (deref_list (r_new.steammons)) then
			Some (Winner Red)
		else if State.all_are_dead (deref_list (b_new.steammons)) then
			Some (Winner Blue)
		else*) None
	in
	let r_data = (deref_list r_new.steammons, deref_list r_new.items) in
	let b_data = (deref_list b_new.steammons, deref_list b_new.items) in
	let set_req t =
		if List.length t.steammons < cNUM_PICKS then
			PickRequest(t.id, (r_data, b_data), !atks, !pool)
		else
			ActionRequest(r_data,b_data)
	in
	let r_req = set_req r_new in
	let b_req = set_req b_new in
	(won, (r_data, b_data) , Some(Request(r_req)), Some(Request(b_req)))

let init_game () =
	let attackify (str : string) : attack =
		let pieces = Str.split (Str.regexp " ") str in
		if List.length pieces <> 8 then
			failwith "invalid attack.txt file - at least one line is wrong"
		else
			let piece = List.nth pieces in
			{name = piece 0; element = type_of_string (piece 1);
			max_pp = int_of_string (piece 2); pp_remaining = int_of_string (piece 2);
			power = int_of_string (piece 3); accuracy = int_of_string (piece 4);
			crit_chance = int_of_string (piece 5);
			effect = (effect_of_num (int_of_string (piece 6)), int_of_string(piece 7))
			}
	in
	let alist  = List.map attackify (List.tl (read_lines "attack.txt")) in
	atks := alist;
	let steammonify (str: string) : steammon =
		let pieces = Str.split (Str.regexp " ") str in
		if List.length pieces <> 13 then
			failwith "invalid steammon.txt file - at least one line is wrong"
		else
			let piece = List.nth pieces in
			{
			  species = piece 0; curr_hp = int_of_string (piece 1);
				max_hp = int_of_string (piece 1);
				first_type = Some (type_of_string (piece 2));
				second_type = if piece 3 = "Nothing" then None
				              else Some (type_of_string (piece 3));
				first_attack = List.hd (List.filter (fun a -> a.name = piece 4) alist);
				second_attack=List.hd (List.filter (fun a -> a.name = piece 5) alist);
				third_attack = List.hd (List.filter (fun a -> a.name = piece 6) alist);
				fourth_attack=List.hd (List.filter (fun a -> a.name = piece 7) alist);
				attack = int_of_string (piece 8); spl_attack = int_of_string(piece 9);
				defense = int_of_string(piece 10);spl_defense =int_of_string(piece 11);
				speed = int_of_string (piece 12); status = [];
				mods = {
					  attack_mod = 1; speed_mod = 1; defense_mod = 1; accuracy_mod = 1
					}
			}
	in
	let slist = List.map steammonify (read_lines "steammon.txt") in
	pool := slist; 
	(*first_pick*)
	let c = if Random.int 2 = 0 then Red else Blue in
	Netgraphics.add_update (SetFirstAttacker c);
	let red = {id = Red; steammons = []; items = []} in
	let blue = {id = Blue; steammons = []; items = []} in
	Netgraphics.send_update InitGraphics;
	((red,blue), c, alist, slist)