open Yojson.Basic.Util
open Card

(** the deck of cards to be used by the game**)
type deck = card list ref

(* the file to initialize the deck *)
let file = Yojson.Basic.from_file "map_info.json"

(* helper function to create (quad * territory_name list) list *)
let makeAssocList label1 label2 yojson_lst =
  let rec makeList acc lst =
    match lst with
    | [] -> acc
    | h::t -> makeList (acc@[h |> member label1 |> to_string,
            h |> member label2 |> to_string]) t
  in makeList [] yojson_lst

(* (quad * territory_name list) list*)
let card_list = makeAssocList "name" "rank" (file |> member "cards" |> to_list)


(** [add_to_end c_lst acc] is a helper function for add_to_end **)
let rec add_to_end c_lst acc =
	match c_lst with
	|[] -> acc
	|h::t -> if List.mem h acc then add_to_end t acc else add_to_end t (acc@[h])

(** [add_cards_to_end c_lst d]
 ** precondition: [c_lst] has type card list
 ** [d] has type deck
 ** postcondition: returns [d] with elements of [c_lst]
 ** included at the very end of it (the order of the newly
 ** added elements is unspecified)
 **)
let add_cards_to_end c_lst d =
	d := add_to_end c_lst (!d)

(** [insert c d i acc] is a helper function for insert_card **)
let rec insert c d i acc =
	match d with
	|[] -> acc@[c]
	|h::t -> if i = 0
	         then acc@[c]@(h::t)
	         else insert c t (i - 1) (acc@[h])

(** [insert_card c d i]
 ** precondition: [c] is a card, [d] is a deck,
 ** [i] is an int that is smaller or equal to the
 ** size of d
 ** postcondition: returns a deck that includes [c]
 ** in [d] at index ([i] - 1) with 0-indexing with
 ** old elements of [d] still contained
 **)
let rec insert_card c d i =
	insert c d i []

(** [shuffle d acc] is a helper function for shuffle_deck **)
let rec shuffle d acc =
	match d with
	|[] -> acc
	|h::t ->
	   let random = if acc = [] then 1 else (Random.int (List.length acc)) in
	   shuffle t (insert_card h acc random)

(** [shuffle_deck d]
 ** precondition: [d] is a deck
 ** postcondition: [shuffle_deck] returns a deck that
 ** contains exacly the same elements as [d] but the
 ** order of the elements may be different
 **)
let shuffle_deck d =
	d := shuffle (!d) []

(** [get_card d]
 ** precondition: [d] is a deck
 ** postcondition: the function returns the first element
 ** of [d], raises failure is [d] is empty
 **)
let get_card d =
	match (!d) with
	|[] -> failwith ("the deck is empty")
	|h::t -> d := t; h

(** [create t_lst acc] is a helper function for create_deck **)
let rec create t_lst acc =
	match t_lst with
	|[] -> acc
	|h::t -> create t ((Card.create_card (fst h) (Pervasives.int_of_string(snd h)))::acc)

(** [create_deck t_lst]
 ** precondition: [t_lst] is a territory_name list
 ** postcondition: returns a deck with cards representing all
 ** the territories with random ranks assigned to each
 **)
let create_deck () =
	ref (create card_list [])