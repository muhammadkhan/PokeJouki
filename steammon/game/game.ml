open Definitions
open Util
open Constants
open Netgraphics

type team = {
	id: color;
	smons: (steammon * int ref) list;
	items: item list
}

(* You have to implement this. Change it from int to yout own state type*)
type game = team * team

let game_datafication g =
	g.teams
	
let game_from_data game_data = 
	failwith "not implemented"

let handle_step g ra ba =
	failwith "implement me!"

let init_game () =
	failwith "implement me!"