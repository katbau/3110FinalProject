exception Illegal
(** variant for the ranks of the cards **)
type rank = Rank_I | Rank_II | Rank_III

(** type definition for territory_name **)
type territory_name = string

(** the card variant **)
type card = Card of territory_name * rank

(** [compare_cards c1 c2] returns an int
 ** compares the ranks of two cards [c1] and [c2]
 ** returns 0 if they are the same
 ** returns a negative int if [c1]is smaller
 ** returns a positive int if [c1] is larger **)
val compare_cards : card -> card -> int

(** [is_redeemable c1 c2 c3] returns a bool
 ** where [c1] [c2] [c3] are cards
 ** makes sure that [c1] [c2] [c3] can be redeemed for points
 ** this requires all three of them to have the same rank or them
 ** to have three different ranks
 ** returns true if redeemable, false if not redeemable **)
val is_redeemable : card -> card -> card -> bool

(** [create_card territory_name rank] returns a card
 ** with given [territory_name] and [rank] **)
val create_card : string -> int -> card

(** [equals c1 c2] returns a bool if [c1] and
 ** [c2] are the exact same card **)
val equals : card -> card -> bool

(** [get_card_name c] returns the territory name
 ** associated with [c] **)
val get_card_name : card -> string

(** [get_card_from_name name c_lst] returns the card with
 ** with a given name from a card list
 ** returns [Failure] if the card is not in the list **)
val get_card_from_name : string -> card list -> card