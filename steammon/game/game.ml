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

let current_out : steammon option ref = ref None

let red_to_start = ref true and blue_to_start = ref true

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
	
(*This function calculates the next iteration for the Team*)	
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
						| PickInventory(inv) ->
							print_string "enter pick inventory";
							{id = old.id; steammons = old.steammons;
							items = reref_list inv}
						| SwitchSteammon(str)
						| SelectStarter(str) ->
							print_endline("enter switch");
							let (newlst, newtl) = List.partition(fun x -> x.species = str) (deref_list old.steammons) in
							let mon = 
								match newlst with
									| [] -> failwith "not a valid steammon"
									| h::_ -> h
							in
							print_endline("exit switch");
							Netgraphics.add_update (SetChosenSteammon (mon.species));
							current_out := Some mon;
							{id = old.id; steammons = reref_list (mon::newtl);
							items = old.items}
						| UseItem(itm, str) -> (
							  let sref=ref(steammon_of_string(deref_list old.steammons) str)in
									match itm with
									| Ether ->
										Item.use_Ether sref;
										State.change_inventory old.items (-1) 0;
										Netgraphics.add_update(PositiveEffect("Ether!", old.id, 0));
										old
									| MaxPotion ->
										Item.use_maxPotion sref; 
										State.change_inventory old.items (-1) 1;
										Netgraphics.add_update(UpdateSteammon(!sref.species, !sref.curr_hp, !sref.max_hp, old.id));
										Netgraphics.add_update(PositiveEffect("used Max Potion", old.id, 0));
										old
									| Revive ->
										Item.use_Revive sref;
										State.change_inventory old.items (-1) 2;
										Netgraphics.add_update(UpdateSteammon(!sref.species, !sref.curr_hp, !sref.max_hp, old.id));
										Netgraphics.add_update(PositiveEffect("Revived!!", old.id, !sref.max_hp / 2));
										old
                  | FullHeal ->
										Item.use_FullHeal sref;
										State.change_inventory old.items (-1) 3;
										Netgraphics.add_update(SetStatusEffects(!sref.species, []));
										Netgraphics.add_update(PositiveEffect("Full Heal!", old.id, 0));
										old
                  | XAttack ->
										Item.use_X_item itm sref;
										State.change_inventory old.items (-1) 4;
										Netgraphics.add_update(PositiveEffect("Attack +1!", old.id, 0));
										old
                  | XDefense ->
										Item.use_X_item itm sref;
										State.change_inventory old.items (-1) 5;
										Netgraphics.add_update(PositiveEffect("Defense +1!", old.id, 0));
										old
                  | XSpeed ->
										Item.use_X_item itm sref;
										State.change_inventory old.items (-1) 6;
										Netgraphics.add_update(PositiveEffect("Speed +1!", old.id, 0));
										old
                  | XAccuracy ->
										Item.use_X_item itm sref;
										State.change_inventory old.items (-1) 7;
										Netgraphics.add_update(PositiveEffect("Accuracy +1!", old.id, 0));
										old
							  )
						| UseAttack(str) ->
							  (*This needs to take effects into account!!*)
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
								let dmg = int_of_float (Attack.final_attack (ref atk) battlemon_ref dfdr) in
								Attack.apply_effect (ref atk) dfdr;
								dfdr := State.change_hp_by !dfdr (-dmg);
								let msg =
									if dmg = 0 then "Miss =("
									else "Hit!"
								in
								let () = if List.mem Poisoned !battlemon_ref.status then
									let dmg = Status.poison_damage battlemon_ref in
									Netgraphics.add_update(UpdateSteammon(!battlemon_ref.species, !battlemon_ref.curr_hp, !battlemon_ref.max_hp, old.id));
									Netgraphics.add_update(NegativeEffect("Poison Damage X_X", old.id, dmg));
									Netgraphics.add_update(Message(battlemon.species ^ " inflicted damage on itself due to poisoning"))
								else () in
								Netgraphics.add_update(UpdateSteammon(!dfdr.species, !dfdr.curr_hp, !dfdr.max_hp, !old2.id));
								Netgraphics.add_update (NegativeEffect(msg, !old2.id, dmg));
								Netgraphics.add_update(Message(battlemon.species ^ " used " ^ atk.name ^ " on " ^ !dfdr.species ^ "!"));
								old	
				)
			| _ -> old	
	

(*All of these should be able to be done if the pool in initialized properly*)
let handle_step g ra ba : game_output =
	let (r_old, b_old) = g in
	
	(*The commanding arguments could also be DoNothing*)
	let r_new = update_team ra r_old (ref b_old) in
	let b_new = update_team ba b_old (ref r_new) in
	let x  = deref_list (r_new.steammons) and y = deref_list (b_new.steammons) in 
	let r_data = (deref_list r_new.steammons, deref_list r_new.items) in
	let b_data = (deref_list b_new.steammons, deref_list b_new.items) in
	let valOf o = match o with Some x -> x | None -> failwith "sdoifjdsiofjdsiof" in
	let set_comm t other_t =
		let team =
			if t.id = Red then "Red" else "Blue"
		in
		(*This sequence needs to be changed, to reflect the picking order*)		
		if List.length t.steammons < cNUM_PICKS  then
		  Some(Request(PickRequest(t.id, (r_data,b_data), !atks, !pool)))
		else if List.length t.steammons = cNUM_PICKS && List.length other_t.steammons < cNUM_PICKS then
			None
		else if t.items = [] then
			(print_endline (team ^ " pokemon done");
			 Some(Request(PickInventoryRequest(r_data,b_data))))
		else if !red_to_start || !blue_to_start || (not (!red_to_start || !blue_to_start) && (!(List.hd t.steammons)).curr_hp = 0) then
			(print_endline (team ^ " has items");
			 let () = if t.id = Red && !red_to_start then
			   red_to_start := false
			 else () in
			 let () = if t.id = Blue && !blue_to_start then
				 blue_to_start := false
				 else () in
			 Netgraphics.add_update(Message("The current dead pokemon on " ^ team ^ " team is " ^ (!(List.hd t.steammons)).species));
			 Some(Request(StarterRequest(r_data,b_data))))
		else 
			(print_endline (team ^ " starter selected");
			Some(Request(ActionRequest(r_data,b_data))))
	in
	let r_comm = set_comm r_new b_new in
	let b_comm = set_comm b_new r_new in
	let r_remaining = List.filter (fun s -> s.curr_hp > 0) x in
	let b_remaining = List.filter (fun s -> s.curr_hp > 0) y in
	let won =
		(*match r_req, b_req with
			| StarterRequest(_),_ -> if (List.filter (fun s -> s.curr_hp > 0) x) = [] then Some (Winner Blue) else None
			| _,StarterRequest(_) -> if (List.filter (fun s -> s.curr_hp > 0) y) = [] then Some (Winner Red) else None
			| _ ->
    		if State.all_are_dead (x)
    		   && State.all_are_dead (y) then
    			Some Tie
    		else if (not(State.all_are_dead y)) && State.all_are_dead (x) then
    			Some (Winner Blue)
    		else if (not (State.all_are_dead x)) && State.all_are_dead (y) then
    			Some (Winner Red)
    		else None*)
				if r_remaining = [] && x <> [] && b_remaining = [] && y <> [] then Some Tie
				else if r_remaining = [] && x <> [] && b_remaining <> [] then
					Some (Winner Blue)
				else if r_remaining <> [] && b_remaining = [] && y <> [] then
					Some (Winner Red)
				else
					None
		in
	(won, (r_data, b_data) , r_comm, b_comm)
	

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
	let c = if (Random.int 2) = 0 then Red else Blue in
	let initItems = [] in
	let red = {id = Red; steammons = []; items = initItems} in
	let blue = {id = Blue; steammons = []; items = initItems} in
	Netgraphics.send_update InitGraphics;
	Netgraphics.add_update (SetFirstAttacker c);
	((red,blue), c, alist, slist)