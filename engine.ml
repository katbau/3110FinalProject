open Map 
open Player



type state = {
  mutable current_player: player;
  mutable players : player list;
  mutable defeated : (player * bool) list;
  initial_order : (player * int) list;
  map : map;
  redeem_num 
}

11 territories = 3
14 territories = 4 
17 territories = 5


let parsed s = 
	String.lowercase_ascii (String.trim (s))

let redeem_tas st = 
	match st.redeem_num with
	| 0 -> 4
	| 1 -> 6
	| 2 -> 8
	| 3 -> 10
	| 4 -> 12
	| 5 -> 15
	| _ -> (st.redeem_num + 5)

let get_new_tas st = 
	let new_tas = 0 in
	let terr_num = controlled_territories st.current_player in
	if terr_num >= 15 then new_tas = 5
	else if terr_num >= 12 then new_tas = 4
	else new_tas = 3 in

	print_endline "Do you want to redeem a set of three cards?" in
	let answer_redeem =  parsed (readline ()) in
	if answer_redeem = yes then 
		print_endline "Name the 3 cards you would like to redeem in the format 
							Name Rank, Name Rank, Name Rank"

	is_redeemable
	redeem
	else 


let is_quad_conquered st =
	get_quad_terr_list st.map


let redeem_cards card1 card2 card3 = 


let attack terr = 




