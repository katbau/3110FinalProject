exception Failure

type territory_name = string
type ta_number = int

(** 'territory_name list' represents this territory's adjacent territories **)
type territory = 
   Territory of territory_name * ta_number ref * territory_name list

(** [make_territory tn ta tl] is a new territory variant 
 ** initialized to those values tn,ta,tl **)
let make_territory terr_name  ta_num terr_names =
  Territory(terr_name, ref ta_num, terr_names)

(** [get_name t] is the name of territory [t]**)
let get_name terr =
  match terr with
  | Territory (n,_,_) -> n

(** [get_tas t] is the amount of TAs in territory [t]**)
let get_tas terr =
  match terr with
  | Territory (_,n,_) -> !n

(** [adjTerr a] is an associative list, (territory_name * territory) list,
 ** contaning every adjacent territory of [a].**)
let get_adj_terr terr =
  match terr with
  | Territory (_,_,lst) -> lst

(** [update_territory terr ta] is a new territory [Territory (tn, ta, tl)]
 ** where [terr] = Territory (tn, 'ta, tl) **)
let update_territory terr num =
  match terr with
  | Territory (_,n,_) -> n := !n + num