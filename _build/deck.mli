open Card

(** the deck of cards to be used by the game**)
type deck = card list ref


(** [add_cards_to_end c_lst d] returns a deck
 ** with cards in c_lst included
 **)
val add_cards_to_end : card list -> deck -> unit

(** [shuffle_deck d] returns a deck
 ** where the order of [d] is randomized **)
val shuffle_deck : deck -> unit

(** [get_card d] returns a card
 ** that is in [d]
 ** raises Failure if [d] is empty **)
val get_card : deck -> card

(** [create_deck t_list d] returns a deck
 ** where it creates a card for every territory
 ** at the start of the game **)
val create_deck : unit -> deck