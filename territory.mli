type territory_name = string
type ta_number = int

exception Failure

(** 'territory_name list' represents this territory's adjacent territories **)
type territory = Territory of territory_name * ta_number ref * territory_name list

(** [make_territory tn ta tl] is a new territory variant initialized to those values tn,ta,tl **)
val make_territory : territory_name -> ta_number -> territory_name list -> territory


(** [get_name t] is the name of territory [t]**)
val get_name : territory -> string


(** [get_tas t] is the amount of TAs in territory [t]**)
val get_tas : territory -> int


(** [adjTerr a] is an associative list, (territory_name * territory) list,
 ** contaning every adjacent territory of [a].**)
val get_adj_terr : territory -> territory_name list


(** [update_territory terr ta] is a new territory [Territory (tn, ta, tl)]
 ** where [terr] = Territory (tn, 'ta, tl) **)
val update_territory : territory -> ta_number -> unit