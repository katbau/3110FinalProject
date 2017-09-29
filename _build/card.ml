exception Illegal
(** variant for the ranks of the cards **)
type rank = Rank_I | Rank_II | Rank_III

(** type definition for territory_name **)
type territory_name = string

(** card variant **)
type card = Card of territory_name * rank

(** [compare_cards c1 c2]
 ** precondition: [c1], [c2] have type card
 ** postcondition: returns 0 if
 ** the ranks of [c1] and [c2] are the same
 ** returns 1 if [c1] has a higher rank
 ** returns -1 if [c1] has a lower rank
 ** the order of ranks from lower to higher is
 ** Rank I < Rank II < Rank III
 *)
let compare_cards c1 c2 =
	match c1,c2 with
	|(Card (_,Rank_I), Card (_,Rank_I)) -> 0
	|(Card (_,Rank_I), Card (_,_)) -> -1
	|(Card (_,Rank_II), Card (_, Rank_I)) -> 1
	|(Card (_,Rank_II), Card (_, Rank_II)) -> 0
	|(Card (_,Rank_II), Card (_,_)) -> -1
	|(Card (_,Rank_III), Card (_,Rank_III)) -> 0
	|(Card (_,Rank_III), Card (_,_)) -> 1

(** [is_redeemable c1 c2 c3]
 ** precondition: [c1],[c2],[c3] have type card
 ** postcondition: returns true if the three input
 ** cards make a redeemable set
 ** the rules of redeeming are explained in detail
 ** in the design document
 **)
let is_redeemable c1 c2 c3 =
	match compare_cards c1 c2,compare_cards c2 c3 with
	|(0,0) -> true
	|(0,_) -> false
	|(_,0) -> false
	|(-1,-1) -> true
	|(1,1) -> true
	|_ -> if compare_cards c1 c3 = 0
	      then false
	      else true


(** [create_card ter_name rank]
 ** precondition: [ter_name] is a valid territory name
 ** in the form of a string
 ** [rank] is an int between 0 and 2 (inclusive)
 ** postcondition: a card with the given territory
 ** name and rank
 **)
let create_card ter_name rank =
	match rank with
	|0 -> Card(ter_name,Rank_I)
	|1 -> Card(ter_name,Rank_II)
	|2 -> Card(ter_name,Rank_III)
	|_ -> raise (Failure "Not a valid rank")

(** [equals c1 c2]
 ** precondition: [c1], [c2] have type card
 ** postcondition: returns true if [c1] and [c2]
 ** are exactly the same
 ** (only checks for the territory name since every
 ** territory only has one card associated with it)
 **)
let equals c1 c2 =
	match c1,c2 with
	|(Card (tn1,_), Card (tn2,_)) -> if tn1 = tn2 then true else false

(** [get_card_name c]
 ** precondition: [c] has type card
 ** postcondition: returns the name of [c]
 **)
let get_card_name c =
	match c with
	| Card(tn,_) -> tn

(** [get_card_from_name name c_lst]
 ** precondition: [name] is a string
 ** [c_lst] is a card list
 ** postcondition: returns the card with the
 ** the given [name] from the [c_lst]
 ** retruns failure if the card is not found
 **)
let rec get_card_from_name name c_lst =
	match c_lst with
	| [] -> raise Illegal
	| (Card(n,r))::t ->
	   if n = name then Card(n,r)
	   else get_card_from_name name t