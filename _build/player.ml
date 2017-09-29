open Card
open Territory

type name = string

(* type player to keep track of player state *)
type player = name * card list ref * territory list ref


(** [create_player name] Creates a record of player to represent a player of game
 ** play initializes a player, their territory list, previous redeems, name,
 ** and card list **)
let create_player name =
  (name,ref [], ref [])

(** [cards_in_hand p] takes player [p] and returns the card list associated
 ** with the player **)
let cards_in_hand player =
  match player with
  | (_,cards,_) -> !cards

(** [is_card_in_hand player card] returns [true] if the player has
 ** [card] in its hand and [false otherwise] **)
let is_card_in_hand player card =
  let cardlist = cards_in_hand player in
  let rec helper_func lst card =
  match lst with
  | [] -> false
  | h::t -> if h = card then true || helper_func t card else false || helper_func t card  in
  helper_func cardlist card

(** [player_name (name,_,_)] returns the name of the player **)
let player_name (name,_,_) =
  name

(** [update_cards p c] player p's card list to include card [c] **)
let update_cards player card_list =
  match player with
  | (_,cards,_) -> cards := (!cards)@card_list

(** helper fuction for [remove_ca] **)
let rec remove_help l1 l2 acc =
  match l1 with
  | [] -> acc
  | h::t -> if List.mem h l2 then remove_help t l2 acc else remove_help t l2 (acc@[h])

(** [remove_cards p lst] updates player [p]'s card list to remove cards in
 ** [lst]. If the card/s in [lst] are not in the card list of [p], [p] will
 ** will remain unchanged. **)
let remove_cards player card_list =
  match player with
  | (_,cards,_) -> cards := remove_help (!cards) card_list []

(** [controlled_territories p] is a list of the territories controlled by [p] **)
let controlled_territories player =
  match player with
  | (_,_,terr) -> !terr

(* [controlled_names p] is a list of the names of the territories controlled
 * by [p] *)
let controlled_names pl =
  let cont = controlled_territories pl in
  let rec get_names acc = function
  | [] -> acc
  | (Territory (n,_,_))::t -> get_names (acc@[n]) t
in get_names [] cont

(** [add_territory p t] updates the territory list of [p] so that it includes
 ** territory [t]. **)
let add_territory player territory =
  match player with
  | (_,_,terr) -> terr := (!terr)@[territory]

(** [remove_territory p t] updates [p] so that it no longer has territory [t]
 ** in its controlled territories list. **)
let remove_territory player territory =
  let rec rem terr = function
  | [] -> []
  | h::t -> if h = terr then t else h::(rem terr t) in
  match player with
  | (_,_,t_lst) -> t_lst := rem territory (controlled_territories player)

(** [num_of_tas m p] takes map m and player p and returns the total number
 ** of TAs that the player has. **)
let num_of_tas player =
  let rec get_tas terrs acc =
  match terrs with
  | [] -> acc
  | h::t -> get_tas t (acc + Territory.get_tas h) in
  get_tas (controlled_territories player) 0