open Card
open Territory

type name = string

(* type player to keep track of player state *)
type player = name * card list ref * territory list ref

(** [create_player name] Creates a record of player to represent a player of game
 ** play initializes a player, their territory list, previous redeems, name,
 ** and card list **)
val create_player: name -> player

(** [player_name (name,_,_)] returns the name of the player **)
val player_name: player -> name

(** [cards_in_hand p] takes player [p] and returns the card list associated
 ** with the player **)
val cards_in_hand: player -> card list

(** [update_cards p c] player p's card list to include card [c] **)
val update_cards: player -> card list -> unit

(** [remove_cards p lst] updates player [p]'s card list to remove cards in
 ** [lst]. If the card/s in [lst] are not in the card list of [p], [p] will
 ** will remain unchanged. **)
val remove_cards : player -> card list -> unit

(** [controlled_territories p] is a list of the territories controlled by [p] **)
val controlled_territories: player -> territory list

(* [controlled_names p] is a list of the names of the territories controlled
 * by [p] *)
val controlled_names : player -> territory_name list

(** [add_territory p t] updates the territory list of [p] so that it includes
 ** territory [t]. **)
val add_territory: player -> territory -> unit

(** [remove_territory p t] updates [p] so that it no longer has territory [t]
 ** in its controlled territories list. **)
val remove_territory: player -> territory -> unit

(** [num_of_tas m p] takes map m and player p and returns the total number
 ** of TAs that the player has. **)
val num_of_tas: player -> int

(** [is_card_in_hand player card] returns [true] if the player has
 ** [card] in its hand and [false otherwise] **)
val is_card_in_hand : player -> card -> bool




