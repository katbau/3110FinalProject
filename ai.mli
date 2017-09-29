open Risk_map
open Territory
open Player
open Card

(** keeps track of the state of the ai **)
type ai = player

(** [create_ai name ] creates a new ai_state with a
 **	new player created with [name] **)
val create_ai : string -> ai

(** [select_terrs pl map empty_terr] chooses the initial territories for ai **)
val select_terrs : ai -> map -> territory list -> territory_name

(** [ai_redeem ai] decides if the [ai] has a redeemable set **)
val ai_redeem : ai -> card list

(** [ai_attack ai m] decides if the [ai] should attack **)
val ai_attack : ai -> map -> (territory * territory) option

(** [reinforce p m] decides the territory to reinforce  **)
val ai_reinforce : ai -> map -> territory

(** [ai_fortify ai m] decides [ai] should fortify **)
val ai_fortify : ai -> map -> (territory * territory * int) option