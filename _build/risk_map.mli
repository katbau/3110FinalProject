open Yojson.Basic.Util
open Territory
open Player

exception Failure

type occupant = Player.name
and quad = string
and map_terr = {territory : territory ref; quad : quad; occupant : occupant ref}
and map = (territory_name * map_terr) list

(** [make_map lst] will create base map that contains all of the
 ** territories in [lst]. Will only be used once in the beginning of
 ** the game. **)
val make_map : unit -> map

(**[update_map m lst] updates the map after every action that is
 ** done in a turn. Takes a map, and (territory, occupant) tuple
 ** that will be used to update 'territory'. **)
val update_map : map -> territory -> occupant -> unit

(** [can_attack a] is [true] if [a] can attack, meaning it has at least 2 TAs
 ** and [adjTerr a] contains enemy territories, [false] otherwise **)
val can_attack : map -> territory -> bool

(** [any_empty map'] is [true] if there are any unoccupied
 ** territories im [map'], [false] otherwise **)
val any_empty: map -> bool

(** [is_not_occupied terr_name map'] is [true] if the occupant
 ** of the territory with the name [terr_name] is an empty string
 ** [false] otherwise **)
val is_not_occupied: string -> map -> bool

(** [get_empty_terrs m] gets the list of territories in [m] that are
 ** unoccupied **)
val get_empty_terrs : map -> territory list

(** [get_quad_terr_list map terrs] returns a associated list of territory names
 ** and the quads that they are in **)
val get_quad_terr_list : map -> territory_name list -> (territory_name * quad) list

(*val occupant: string -> map -> name -> bool *)
val return_quad_list: unit -> (string * string list) list
