open Risk_map
open Territory
open Player
open Card

(** keeps track of the player of the ai **)
type ai = player

(** [create_ai name game_map]
 ** precondition: [name] is a string
 ** postcondition: returns a new ai state
 ** with newly created player with [name] as
 ** its player name **)
let create_ai name =
  create_player name

(** [check_for_redee9m c_lst] is a function that checks if
 ** the ai_state has a redeemable set of cards
 ** if there are two redeemable sets, it is not specified
 ** which one will be returned
 ** precondition: [c_lst] has type card list
 ** postcondition: returns a pair of bool and a card list option
 ** if redeemable then the bool is [true] and card list option is
 ** the [Some] three cards to be redeemed
 ** else the bool is [false] and the card list option is [None] **)
let check_for_redeem c_lst =
  match c_lst with
  |[] -> (false, None)
  |[c1] -> (false, None)
  |[c1;c2] -> (false,None)
  |[c1;c2;c3] -> (is_redeemable c1 c2 c3, Some c_lst)
  |[c1;c2;c3;c4] ->
    if (is_redeemable c1 c2 c3) then (true,Some[c1;c2;c3])
    else if (is_redeemable c1 c2 c4) then (true,Some[c1;c2;c4])
    else if (is_redeemable c1 c3 c4) then (true,Some[c1;c3;c4])
    else if (is_redeemable c2 c3 c4) then (true,Some[c2;c3;c4])
    else (false,None)
  |[c1;c2;c3;c4;c5] ->
    if (is_redeemable c1 c2 c3) then (true,Some[c1;c2;c3])
    else if (is_redeemable c1 c2 c4) then (true,Some[c1;c2;c4])
    else if (is_redeemable c1 c2 c5) then (true,Some[c1;c2;c5])
    else if (is_redeemable c1 c3 c4) then (true,Some[c1;c3;c4])
    else if (is_redeemable c1 c3 c5) then (true,Some[c1;c3;c5])
    else if (is_redeemable c1 c4 c5) then (true,Some[c1;c4;c5])
    else if (is_redeemable c2 c3 c4) then (true,Some[c2;c3;c4])
    else if (is_redeemable c2 c4 c5) then (true,Some[c2;c4;c5])
    else if (is_redeemable c3 c4 c5) then (true,Some[c3;c4;c5])
    else (false,None)
  | _ -> (false, None)

(** [check_adj_for_reinforce adj_ters terr p_name m acc] helper function
 ** for [terr_to_fortify] that checks [adj_ters] to decide **)
let rec check_adj_for_reinforce adj_ters terr p_name m acc =
  match adj_ters with
  | [] -> acc
  | h::t -> let adj_terr = List.assoc h m in
              if p_name = !(adj_terr.occupant)
              then check_adj_for_reinforce t terr p_name m acc
              else if get_tas terr < get_tas !(adj_terr.territory)
              then check_adj_for_reinforce t terr p_name m true
              else check_adj_for_reinforce t terr p_name m acc

(** [terr_to_fortify_from terrs ter] finds a territory to fortify from **)
let rec terr_to_fortify_from terrs ter =
  match terrs with
  | [] -> None
  | h::t ->
     if (get_tas ter < get_tas h)
     then Some h
     else terr_to_fortify_from t ter

(** [ter_to_fortiy terrs acc p_name m] helper function for
 ** [check_fortify] to find a territory that needs fortification **)
let rec terr_to_fortify terrs acc p_name m =
    match terrs with
    | [] -> None
    | h::t ->
        if check_adj_for_reinforce (get_adj_terr h) h p_name m false
        then (match terr_to_fortify_from acc h with
              | None -> terr_to_fortify t acc p_name m
              | Some a ->  Some (h,a,((get_tas a)/2)))
        else terr_to_fortify t acc p_name m

(** [check_fortify p m] decides if ai should fortify **)
let check_fortify p m =
  let terrs = (controlled_territories p) in
  terr_to_fortify terrs terrs (player_name p) m

(* assigns a value to [terr] based in how strategicallt advantageous it is. *)
let terr_value map terr cont =
  let rec counter n = function
  | [] -> n
  | (nm,q)::t -> if List.mem nm (fst (List.split cont))
                    then if List.mem_assoc (get_name terr) cont && q = (List.assoc (get_name terr) cont)
                      then counter (n+2) t
                      else counter (n + 1) t
                 else counter n t
  in counter 0 (get_quad_terr_list map (get_adj_terr terr))

(* helper function for select_terrs *)
let rec optimum_terr map terr_lst acc cont =
  match terr_lst with
  | [] -> acc
  | h::t -> if terr_value map h cont > terr_value map acc cont
              then optimum_terr map t h cont
            else optimum_terr map t acc cont

(** [select_terrs pl map empty_terr] chooses the initial territories for ai **)
let select_terrs pl map empty_terr =
  let cont_terr = controlled_names pl in
  let cont = get_quad_terr_list map cont_terr in
  if cont = [] then
    get_name (List.nth empty_terr (Random.int (List.length empty_terr)))
  else
    get_name (optimum_terr map empty_terr (List.hd empty_terr) cont)

(** [terr_to_reinforce terrs p_name m] checks the [terrs] of
 ** the ai to decide which territory needs reinforcement **)
let rec terr_to_reinforce terrs p_name m =
  match terrs with
  | [] -> None
  | h::t ->
    if check_adj_for_reinforce (get_adj_terr h) h p_name m false
    then Some h
    else terr_to_reinforce t p_name m

(** [check_adj_terrs terr adj_terrs p_name m] checks the [adj_terrs]
 ** of a [terr] to decide if ai should attack **)
let rec check_adj_terrs terr adj_terrs p_name m =
  match adj_terrs with
  | [] -> None
  | h::t -> let adj_terr = List.assoc h m in
            if p_name = !(adj_terr.occupant)
            then check_adj_terrs terr t p_name m
            else if get_tas terr + 1 > get_tas !(adj_terr.territory)
            then Some !(adj_terr.territory)
            else check_adj_terrs terr t p_name m

(** [check_attack_for_ter h p_name m] is a helper function for
 ** [decide_if_attack] that checks individual territories **)
let check_attack_for_ter h p_name m =
  let adj_terrs = get_adj_terr h in
  check_adj_terrs h adj_terrs p_name m

(** [decide_if_attack p_ters p_name m] checks the current [m]
 ** and the [p_ters] to decide if ai should attack **)
let rec decide_if_attack p_ters p_name m =
  match p_ters with
  |[] -> (false,None)
  |h::t -> if can_attack m h
           then (match check_attack_for_ter h p_name m with
                 | None -> decide_if_attack t p_name m
                 | Some a -> (true,Some (h,a)))
           else decide_if_attack t p_name m

(** [ai_fortify ai m] decides [ai] should fortify
 ** precondition: [ai] has type ai and [m] has type map
 ** postcondition: returns a Some (territory * territory * int)
 ** where the first element is the territory to be fortified and
 ** the second the the territory to be fortified from and the int the
 ** number to be fortified
 ** or None if does not want to fortify **)
let ai_fortify ai m =
  check_fortify ai m

(** [ai_redeem ai] decides if the [ai] has a redeemable set
 ** precondition: [ai] has type ai
 ** postcondition: returns a card list with exactly three elements
 ** that are the redeemable set or an empty set **)
let ai_redeem ai =
  match check_for_redeem (cards_in_hand ai) with
    | (true,Some c_lst) -> c_lst
    | _ -> []

(** [ai_attack ai m] decides if the [ai] should attack
 ** precondition: [ai] has type ai, [m] has type map
 ** postcondition: returns a territory pair option if [ai] should
 ** attack, None if not **)
let ai_attack ai m =
  match decide_if_attack (controlled_territories ai) (player_name ai) m with
  |(true,Some (h,a)) -> Some (h,a)
  |_ -> None

(** [reinforce p m] decides the territory to reinforce
 ** precondition: [p] has type ai, [m] has type map
 ** postcondition: returns a territory **)
let rec ai_reinforce p m  =
   match terr_to_reinforce (controlled_territories p) (player_name p) m with
   | None -> List.hd (controlled_territories p)
   | Some a -> a