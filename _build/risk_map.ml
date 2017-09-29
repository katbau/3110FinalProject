open Yojson.Basic.Util
open Territory
open Player

exception Failure

(* game will keep track of redeems*)
type occupant = Player.name
and quad = string
and map_terr = {territory : territory ref; quad : quad; occupant : occupant ref}
and map = (territory_name * map_terr) list

(** file to initialize game **)
let file = Yojson.Basic.from_file "map_info.json"

(* returns an association list made from the yojson file, using [label1] and
 * [label 2] to get the information.*)
let makeAssocList label1 label2 yojson_lst =
  let rec makeList acc lst =
    match lst with
    | [] -> acc
    | h::t -> makeList (acc@[h |> member label1 |> to_string,
            h |> member label2 |> to_list |> filter_string]) t
  in makeList [] yojson_lst

(* (quad * territory_name list) list*)
let quads = makeAssocList "name" "territories" (file |> member "quads" |> to_list)

(* (territory_name * territory_name list) list *)
let adjacents = makeAssocList "name" "adjacent" (file |> member "territories" |> to_list)

(* takes an accumulator and two lists as input, returns list with no duplicates,
 * containing every element in both lists.*)
let rec combine acc = function
| [] -> List.sort Pervasives.compare acc
| h::t -> if List.mem h t then combine acc t else combine (h::acc) t

(* adds list to map*)
let rec addList map qd = function
| [] -> map
| h::t -> let terr = make_territory h 0 (combine [] ((List.assoc qd quads)@(List.assoc h adjacents))) in
          addList ((h,{territory = ref terr; quad = qd; occupant = ref ""})::map) qd t

(* helper function for [make_map] *)
let rec make_map_h quad_lst map =
  match quad_lst with
  | [] -> map
  | h::t -> make_map_h t (addList map (fst h) (snd h))

(* returns the list of quads of the game *)
let return_quad_list () = 
  quads

(** [make_map ()] will create base map that contains all of the
 ** territories in [lst]. Will only be used once in the beginning of
 ** the game. **)
let make_map () =
  make_map_h quads []

(**[update_map m lst] updates the map after every action that is
 ** done in a turn. Takes a map, and (territory, occupant) tuple
 ** that will be used to update 'territory'.**)
let update_map m t' occ =
  let rec update_terr terr = function
  | [] -> raise Failure
  | h::t -> if fst h = Territory.get_name terr
            then (match (snd h).territory := terr with _ -> (snd h).occupant := occ) else update_terr terr t
  in update_terr t' m

(** [can_attack a] is [true] if [a] can attack, meaning it has at least 2 TAs
 ** and [adjTerr a] contains enemy territories, false otherwise**)
let can_attack map terr =
  if Territory.get_tas terr >= 2
    then
      let terr_adj = List.assoc (Territory.get_name terr) adjacents in
      let rec has_enemy acc = function
      | [] -> acc
      | h::t -> let this_occ = (List.assoc (get_name terr) map).occupant in
                let enemy_occ = (List.assoc h map).occupant in
                let is_enemy = (!this_occ <> !enemy_occ) && (!enemy_occ <> "") in
                has_enemy (acc || is_enemy) t
      in has_enemy false terr_adj
    else
      false

(** [any_empty map'] is [true] if there are any unoccupied
 ** territories im [map'], [false] otherwise **)
let any_empty map' =
  List.exists (fun (a,b) -> !(b.occupant) = "") map'

(** [is_not_occupied terr_name map'] is [true] if the occupant
 ** of the territory with the name [terr_name] is an empty string
 ** [false] otherwise **)
let is_not_occupied terr_name map' =
  let mp_terr = List.assoc terr_name map' in
  !(mp_terr.occupant) = ""

(* helper function for [get_empty_terrs] *)
let get_territories m =
  let terrs = snd (List.split m) in
  let rec extract_terrs acc = function
  | [] -> acc
  | h::t -> extract_terrs (!(h.territory)::acc) t
  in extract_terrs [] terrs

(** [get_empty_terrs m] gets the list of territories in [m] that are
 ** unoccupied **)
let get_empty_terrs m =
  let terrs = get_territories m in
  List.filter (fun t -> is_not_occupied (get_name t) m) terrs

(** [get_quad_terr_list map terrs] returns a associated list of territory names
 ** and the quads that they are in **)
let get_quad_terr_list map terrs =
  let rec get_quads acc = function
  | [] -> acc
  | h::t -> get_quads ((h,(List.assoc h map).quad)::acc) t
in get_quads [] terrs
