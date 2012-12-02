open Definitions
open Util
open Constants
open Netgraphics
open State

type team = {
	id: color;
	steammons: steammon ref list;
	items: int ref list (*mutable inventory*)
}

(*first team = red, second team = blue*)
type game = team ref * team ref

let pool = ref []

let atks = ref []

let first_pick = ref true

let red_to_start = ref true and blue_to_start = ref true

let pick_num = ref (-1)

let first_color = ref Red

let game_datafication g : game_status_data =
	let (redTeam, blueTeam) = g in
	((deref_list !redTeam.steammons, deref_list !redTeam.items),
	(deref_list !blueTeam.steammons, deref_list !blueTeam.items))

let game_from_data game_data : game = 
	let ((r_slist, r_inv), (b_slist, b_inv)) = game_data in
	let red_team = ref {id = Red; steammons = reref_list r_slist;
	    items = reref_list r_inv} in
	let blue_team = ref {id = Blue; steammons = reref_list b_slist;
	    items = reref_list b_inv} in
	(red_team, blue_team)
	
(*This function calculates the next iteration for the Team*)	
let rec update_team cmd (old : team) (old2 : team ref) : team =
		match cmd with
		| Action(act) -> (
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
						{id = old.id; steammons = old.steammons;
						items = reref_list inv}
				| SwitchSteammon(str) ->
						let h = List.hd old.steammons in
						h := State.change_status_list !h (List.filter (fun x -> x <> Confused) !h.status);
						List.iter (State.change_mods_by h 1) [1;2;3;4];
						update_team (Action(SelectStarter str)) old old2
				| SelectStarter(str) ->
						let (newlst, newtl) = List.partition(fun x -> x.species = str) (deref_list old.steammons) in
						let mon = 
							match newlst with
							| [] -> failwith "not a valid steammon"
							| h::_ -> h
						in
						Netgraphics.add_update (SetChosenSteammon (mon.species));
						{id = old.id; steammons = reref_list (mon::newtl);
						items = old.items}
				| UseItem(itm, str) -> (
						let sref = List.find (fun r -> !r.species = str) old.steammons in
							match itm with
							| Ether ->
								  let ar1 = ref (!sref.first_attack) in
  								let ar2 = ref (!sref.second_attack) in
  								let ar3 = ref (!sref.third_attack) in
  								let ar4 = ref (!sref.fourth_attack) in
  								List.iter Item.use_Ether [ar1;ar2;ar3;ar4];
  								State.change_inventory old.items (-1) 0;
  								Netgraphics.add_update(PositiveEffect("Ether!", old.id, 0));
  								Netgraphics.add_update(Message(!sref.species ^ " used Ether!"));
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
						let first_atk = ref false in
						let second_atk = ref false in
						let third_atk = ref false in
						let fourth_atk = ref false in
						let atk1 = ref !battlemon_ref.first_attack in
						let atk2 = ref !battlemon_ref.second_attack in
						let atk3 = ref !battlemon_ref.third_attack in
						let atk4 = ref !battlemon_ref.fourth_attack in
						let atk : attack ref =
							if !atk1.name = str then (first_atk := true; atk1)
							else if !atk2.name = str then (second_atk := true; atk2)
							else if !atk3.name = str then (third_atk := true; atk3)
							else if !atk4.name = str then (fourth_atk := true; atk3)
							else failwith "not a valid attack"
						in
						State.change_pp_by atk (-1);
						State.update_steammon_attack battlemon_ref !atk (
							  if !first_atk then 1 else if !second_atk then 2
								else if !third_atk then 3 else if !fourth_atk then 4
								else failwith "not supposed to happen YO"
							);
						let dfdr = List.hd (!old2.steammons) in
						let dmg = int_of_float (Attack.final_attack atk battlemon_ref dfdr) in
						Attack.apply_effect atk dfdr;
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
						Netgraphics.add_update(Message(battlemon.species ^ " used " ^ !atk.name ^ " on " ^ !dfdr.species ^ "!"));
						old	
				)
			| _ -> old	
	

(*All of these should be able to be done if the pool in initialized properly*)
let handle_step g ra ba : game_output =
	let (r_old, b_old) = g in
	let () = if List.length !r_old.steammons < cNUM_PICKS || List.length !b_old.steammons < cNUM_PICKS then
    		(r_old := update_team ra !r_old b_old;
    		b_old := update_team ba !b_old r_old;)
	else
		let modified_speed (t : team) =
			if t.steammons = [] then failwith "shouldn't be here!"
			else
				let p = List.hd t.steammons in
				(float_of_int !p.speed)*.(mod_constant_spd !p.mods.speed_mod)
		in
		if modified_speed (!r_old) < modified_speed (!b_old) then
			(Netgraphics.add_update(SetFirstAttacker Blue);
			b_old := update_team ba !b_old r_old;
			r_old := update_team ra !r_old b_old;)
		else
			(Netgraphics.add_update(SetFirstAttacker Red);
			r_old := update_team ra !r_old b_old;
    	b_old := update_team ba !b_old r_old;)
	in
	let x  = deref_list (!r_old.steammons) and y = deref_list (!b_old.steammons) in 
	let r_data = (x, deref_list !r_old.items) in
	let b_data = (y, deref_list !b_old.items) in
	let set_comms (red : team) (blue : team) : (command option) * (command option) =
		incr(pick_num);
		let (f,s) = match !first_color with
			| Red -> (red, blue)
			| Blue -> (blue, red)
		in
		if (!pick_num / 2) mod 2 = 1 && List.length f.steammons < cNUM_PICKS then
			(None, Some(Request(PickRequest(f.id, (r_data,b_data), !atks, !pool))))
		else if (!pick_num / 2) mod 2 = 0 && List.length s.steammons < cNUM_PICKS then
			 (Some(Request(PickRequest(s.id, (r_data,b_data), !atks, !pool))), None)
		else if red.items = [] && blue.items = [] then
			(Some(Request(PickInventoryRequest(r_data,b_data))), Some(Request(PickInventoryRequest(r_data,b_data))))
		else if !red_to_start && !blue_to_start then
			(red_to_start := false; blue_to_start := false;
			(Some(Request(StarterRequest(r_data,b_data))), Some(Request(StarterRequest(r_data,b_data))))
			)
		else
			(
				let r = (ref (Some(Request(ActionRequest(r_data,b_data)))),
				           ref (Some(Request(ActionRequest(r_data,b_data))))) in
				let () =
					if !(List.hd red.steammons).curr_hp = 0 then
						(fst r) := Some(Request(StarterRequest(r_data,b_data)))
					else ()
				in
				let () =
					if !(List.hd blue.steammons).curr_hp = 0 then
						(snd r) := Some(Request(StarterRequest(r_data,b_data)))
					else ()
				in
				(!(fst r), !(snd r))
			)
	in
	let (r_comm, b_comm) = set_comms !r_old !b_old in
	let r_remaining = List.filter (fun s -> s.curr_hp > 0) x in
	let b_remaining = List.filter (fun s -> s.curr_hp > 0) y in
	let won =
		if r_remaining = [] && x <> [] && b_remaining = [] && y <> [] then Some Tie
		else if r_remaining = [] && x <> [] && b_remaining <> [] then
			Some (Winner Blue)
		else if r_remaining <> [] && b_remaining = [] && y <> [] then
			Some (Winner Red)
		else
			None
		in
	(won, (r_data, b_data) , r_comm, b_comm)
	


(*The initialization of our game*)
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
	first_color := c;
	let initItems = [] in
	let red = ref {id = Red; steammons = []; items = initItems} in
	let blue = ref {id = Blue; steammons = []; items = initItems} in
	Netgraphics.send_update InitGraphics;
	(*Netgraphics.add_update (SetFirstAttacker c);*)
	((red,blue), c, alist, slist)
