open Territory
open Player
open Deck
open Card
open Risk_map
open Ai
open Dice
open Str
open Risk_Gui

exception Illegal

type state = Risk_Gui.state

(*[new_name name lst] returns true if name is not in lst and false
if name is in list*)
let not_used name lst =
 let comps = ["Computer 1";"Computer 2";"Computer 3"] in
 let rec not_used_helper = function
  | [] -> not (List.mem name comps)
  | h::t -> h <> name && not (List.mem name comps) && not_used_helper t in
 not_used_helper lst

(* [roll] determines the die roll *)
let roll () =
  (Random.int 6) + 1

(* returns [true] if the given player is an ai, [false] othewise *)
let is_ai pl =
match pl with
| "Computer 1" | "Computer 2" | "Computer 3" -> true
| _ -> false

(* determines the names for the ai players *)
let get_ai_names num acc =
  match num with
  | 0 -> acc
  | 1 -> "Computer 1"::acc
  | 2 -> "Computer 2"::"Computer 1"::acc
  | 3 -> "Computer 3"::"Computer 2"::"Computer 1"::acc
  | _ -> acc

(*[get_names human_number n acc] returns string list of unique names
of size human_number*)
let rec get_names human_number n acc =
  match n with
  | 0 -> List.rev (get_ai_names (4-human_number) acc)
  | i ->  print_endline ("\nPlease enter the player name for player "
          ^ string_of_int (human_number - n + 1));
          print_string  "> ";
          let name = read_line () in
          if String.lowercase_ascii (String.trim (name)) <> "quit"
          then
          (
            if not_used name acc
            then get_names human_number (n - 1) (name::acc)
            else (print_endline ("\n Name is already used or is invalid. " ^
                          "Please choose again.") ;
                get_names human_number n acc)
          )
          else failwith "quit"

(*[player_rolls_die lst] returns list of tuples of a random die roll and
the player name respectively*)
let player_rolls_die lst =
  let rec first_rolls lst accu =
    match lst with
    | [] -> accu
    | h::t -> first_rolls t (accu @ [(roll (), h)]) in
  let results = first_rolls lst [] in
  print_endline "";
  let print_result (roll, name) =
   print_endline (name ^ " rolled a " ^ string_of_int roll) in
  List.iter print_result results;
  results

(*[new_order_of_players lst gives the new order of the players based on
their die rolls*)
let players_in_order lst =
  let comp (a,b) (c,d) = if a < c then 1 else if a > c then -1 else 0 in
  snd List.(lst |> sort comp |> split)

(* [print_order lst] prints the order of the players after the inifial roll *)
let print_order [a;b;c;d] =
  print_endline ("\n" ^ a ^ " goes first, then " ^ b ^
    ", then " ^ c ^", and finally " ^ d)

(* create player types for each player*)
let rec create_player_list lst =
  match lst with
  | [] -> []
  | h::t -> let p = if is_ai h then create_ai h else create_player h
            in p::(create_player_list t)

(* [init_state] creates the initial state from a given [name_list] *)
let init_state name_list =
  let pl = create_player_list name_list in
  {
    players = pl; map = make_map (); current_player = List.hd pl;
    defeated = List.fold_right (fun s acc -> (s,false)::acc) pl [];
    initial_order = List.fold_right2
      (fun p ord acc -> (p,ord)::acc) pl [1;2;3;4] [];
    redeem_num = 0;
    deck = create_deck ()
  }

(* updates the player list in the game *)
let update_player_list st =
  st.players <- List.tl(st.players)@[List.hd(st.players)]

(* updates the player that is currently playing (manages turns) *)
let update_curr_player st =
  st.current_player <- List.hd st.players

(* removes player when they don't have any territories left *)
let remove_player st player =
  let rec list_wo_player st player accu=
  match st.players with
  | [] -> accu
  | h::t -> if (player_name h <> player)
            then list_wo_player st player accu@[h]
            else list_wo_player st player accu in
  st.players <- list_wo_player st player []

(* checks if there is anyone with no territories (defeated) *)
let is_anyone_defeated st =
  let rec helper_func lst acc =
  match lst with
  | [] -> acc
  | h::t ->
    if controlled_territories h = []
    then helper_func t ([h]@acc)
    else helper_func t acc in
helper_func st.players []

(* updates the game when a player is defeated *)
let update_defeated st player =
  let rec helper_func lst acc =
  match lst with
  | [] -> acc
  | (x,y)::t ->
    if player = x
    then helper_func t ([(x,true)]@acc)
    else helper_func t ([(x,y)]@acc) in
helper_func st.defeated []

(* returns [true] if a player tries to reinforce before occupy step is done
 * [false] otherwise *)
let cannot_reinforce st =
  print_endline ("\n\t--- Cannot Reinforce Before All" ^
    " Territories Are Occupied ---") ; raise Illegal

(* returns [true] if a player tries to occuoy an already occupied territory
 * [false] otherwise *)
let cannot_put_soldier st =
  print_endline "\n\t--- Territory Already Occupied ---"; raise Illegal

(* returns [true] if a command is not valid, [false] otherwise *)
let not_a_command st =
  print_endline "\n\t--- Invalid Command ---"; raise Illegal

(* handles the initial occupying of the map *)
let occupy terr_name currplayer st =
  let terr = !((List.assoc terr_name st.map).territory) in
  if is_not_occupied terr_name st.map
  then (add_territory currplayer terr;
        update_map st.map terr (player_name currplayer);
        update_territory terr 1;
        update_player_list st; update_curr_player st;)
  else (
    if !((List.assoc terr_name st.map).occupant) = player_name currplayer
    then
    (if any_empty st.map
      then cannot_reinforce st
      else update_territory terr 1;
        update_player_list st; update_curr_player st
    )
    else cannot_put_soldier st)

(* shows avalable steps *)
let show_help () =
  let help = [" available - shows empty territories";
              (" auto - automatically assigns initial " ^
                            "tas during initial game sequence");
              " quit - stops game";]
  in print_endline ""; List.iter (fun h -> print_endline h) help

(* parses the string command to be used in the game *)
let parsed s =
  String.lowercase_ascii (String.trim (s))

(* calculates the number of soldiers to be gained by redeeming *)
let redeem_tas st =
  match st.redeem_num with
  | 0 ->  st.redeem_num <- (st.redeem_num + 1); 4
  | 1 ->  st.redeem_num <- (st.redeem_num + 1); 6
  | 2 ->  st.redeem_num <- (st.redeem_num + 1); 8
  | 3 ->  st.redeem_num <- (st.redeem_num + 1); 10
  | 4 ->  st.redeem_num <- (st.redeem_num + 1); 12
  | 5 ->  st.redeem_num <- (st.redeem_num + 1); 15
  | _ ->  st.redeem_num <- (st.redeem_num + 1); (st.redeem_num + 5)

(* calculates the number of quads a player has to be used in TA calculations *)
let num_quads_conquered st =
  let quad_list = return_quad_list () in
  let rec help_func lst accnum st=
  match lst with
  | [] -> accnum
  | (x,[t1;t2;t3;t4;t5])::t ->
    if !((List.assoc t1 st.map).occupant) = (player_name st.current_player) &&
       !((List.assoc t2 st.map).occupant) = player_name st.current_player &&
       !((List.assoc t3 st.map).occupant) =  player_name st.current_player &&
       !((List.assoc t4 st.map).occupant) = player_name st.current_player &&
       !((List.assoc t5 st.map).occupant) = player_name st.current_player
       then help_func t (accnum+1) st else help_func t accnum st in
    help_func quad_list 0 st

(* returns [false] if the input is ["yes"] or ["no"], [false] otherwise *)
let not_yes_or_no st =
  print_endline "\n\t--- Invalid Entry, Answer y or yes / n or no ---";
  raise Illegal

(* prompts the user if the card that is trying to be used is
 * not in player's hand *)
let card_not_in_hand () =
  print_endline "\n\t--- Card Not In Hand ---"; raise Illegal

(* returns [true] if player has 3 or more cards, [false] otherwise *)
let can_blind_redeem st =
  let cards = cards_in_hand st.current_player in
  if List.length cards >= 3 then true else false

(* redeems the cards given a card name list *)
let rec redeem_card_names n accnum acclist st =
 match n with
 | 0 -> acclist
 | i ->
  if can_blind_redeem st then(
    print_endline ("type in the teritory of card" ^ string_of_int accnum
      ^ "that you would like to redeem");
    try
    let answer = parsed(read_line()) in
    let card = get_card_from_name answer (cards_in_hand st.current_player) in
    if is_card_in_hand st.current_player card
    then
      redeem_card_names (n-1) (accnum+1) ([card]@acclist) st
    else card_not_in_hand ()
  with
  |Illegal -> redeem_card_names n accnum acclist st)
else []

(* calculates the number of TAs to be gained at the start of each turn *)
let rec get_new_tas st =
  let new_tas = ref 0 in
  let terr_num = List.length (controlled_territories st.current_player) in
  let () = if terr_num >= 15 then new_tas := 5
          else if terr_num >= 12 then new_tas := 4
          else new_tas := 3 in
  let () = new_tas := (!new_tas + ((num_quads_conquered st) * 5)) in
  if is_ai (player_name st.current_player) then
    (match ai_redeem st.current_player with
    | [] -> !new_tas
    | [c1;c2;c3] -> if is_redeemable c1 c2 c3
                    then (add_cards_to_end [c1;c2;c3] st.deck;
                    remove_cards st.current_player [c1;c2;c3];
                    new_tas:= !new_tas + (redeem_tas st);
                    !new_tas)
                    else !new_tas)
  else (
     print_endline ("Do you want to redeem a set of " ^
      "three cards? Answer Yes\\No");
     let answer_redeem = parsed(read_line()) in
     if answer_redeem = "yes"
     then (let cardlist = redeem_card_names 3 1 [] st in
          if cardlist <> []
          then (
          if is_redeemable (List.hd cardlist)
            (List.nth cardlist 1) (List.nth cardlist 2)
          then (add_cards_to_end cardlist st.deck;
                remove_cards st.current_player cardlist;
                new_tas := !new_tas + (redeem_tas st);
                !new_tas)
          else (print_endline "\n\t--- Cards not redeemable---" ;
                get_new_tas st))
          else !new_tas)
     else if answer_redeem = "no"
     then !new_tas
     else (print_endline "\n\t--- Invalid Entry ---"; get_new_tas st))

(* gets the player with the given [name] *)
let rec find_player st name =
  List.find (fun x -> player_name x = name) st.players

(* returns [true] if the territory with [terr_name] from [lst],
 * [false] otherwise *)
let rec find_in_list lst terr_name =
  match lst with
  | [] -> false
  | h::t -> terr_name = h || find_in_list t terr_name

(* returns [true] if [a] and [d] are adjacent, [false] otherwise *)
let rec adjacent a d =
  let adj_list = get_adj_terr a in
  find_in_list adj_list (get_name d)

(* gets [n] rolls and returns a sorted list *)
let rec get_roll_die_list lst n =
  match n with
  | 0 -> List.rev (List.sort Pervasives.compare lst)
  | i -> get_roll_die_list ([roll_die ()] @ lst) (n-1)

(* prompts the user if a new territory is conquared *)
let rec conquered_a_terr attack_terr def_terr=
  try
      print_endline ("How many TAs would you like to move from"
        ^ get_name def_terr ^ "to your newly conquered territory?");
      let num_to_move = (int_of_string (parsed (read_line ()))) in
      if num_to_move < (get_tas attack_terr - 1)
      then (update_territory attack_terr (-num_to_move);
        update_territory def_terr (num_to_move))
      else (print_endline "\n\t--- Invalid Num of TAs ---";
        conquered_a_terr attack_terr def_terr)
  with
    | _ -> (print_endline "\n\t--- Invalid Num of TAs ---";
        conquered_a_terr attack_terr def_terr)

(* helper function for attack to compare rolled dice *)
let attack_helper_1 atk_die_list def_die_list def atk =
  match Pervasives.compare (List.hd atk_die_list) (List.hd def_die_list)with
  | 1 -> update_territory def (-1); print_endline "defense lost 1 TA"
  | -1 -> update_territory atk (-1);  print_endline "attack lost 1 TA"
  | 0  -> update_territory atk (-1); print_endline "attack lost 1 TA"

(* helper function for attack to compare rolled dice *)
let attack_helper_2 atk_die_list def_die_list def atk =
  match Pervasives.compare (List.hd atk_die_list) (List.hd def_die_list),
  Pervasives.compare (List.nth atk_die_list 1) (List.nth def_die_list 1) with
  | (1,1) -> update_territory def (-2); print_endline "defense lost 2 TAs"
  | (1,-1) -> update_territory def (-1); update_territory atk (-1);
              print_endline "defense lost 1 TA and attack lost 1 TA"
  | (1,0) -> update_territory def (-1); update_territory atk (-1);
              print_endline "defense lost 1 TA and attack lost 1 TA"
  | (-1,0) -> update_territory atk (-2); print_endline "attack lost 2 TAs"
  | (-1, -1) -> update_territory atk (-2); print_endline "attack lost 2 TAs"
  | (-1,1) -> update_territory atk (-1); update_territory def (-1);
              print_endline "defense lost 1 TA and attack lost 1 TA"
  | (0, -1) -> update_territory atk (-2); print_endline "attack lost 2 TAs"
  | (0,0) -> update_territory atk (-2); print_endline "attack lost 2 TAs"
  | (0,1) -> update_territory atk (-1); update_territory def (-1);
             print_endline "defense lost 1 TA and attack lost 1 TA"

(* gets the result of an attack *)
let attack_compare_results atk def =
  match (get_tas atk, get_tas def) with
        | (2,1) -> let atk_die_list = get_roll_die_list [] 1 in
                   let def_die_list = get_roll_die_list [] 1 in
                   draw_dice atk_die_list def_die_list;
                   attack_helper_1 atk_die_list def_die_list def atk
        | (2,_) -> let atk_die_list = get_roll_die_list [] 1 in
                   let def_die_list = get_roll_die_list [] 2 in
                   draw_dice atk_die_list def_die_list;
                   attack_helper_1 atk_die_list def_die_list def atk
        | (3,1) -> let atk_die_list = get_roll_die_list [] 2 in
                   let def_die_list = get_roll_die_list [] 1 in
                   draw_dice atk_die_list def_die_list;
                   attack_helper_1 atk_die_list def_die_list def atk
        | (3,_) -> let atk_die_list = get_roll_die_list [] 2 in
                   let def_die_list = get_roll_die_list [] 2 in
                   draw_dice atk_die_list def_die_list;
                   attack_helper_2 atk_die_list def_die_list def atk
        | (_, 1) -> let atk_die_list = get_roll_die_list [] 3 in
                   let def_die_list = get_roll_die_list [] 1 in
                   draw_dice atk_die_list def_die_list;
                   attack_helper_1 atk_die_list def_die_list def atk
        | (_,_) -> let atk_die_list = get_roll_die_list [] 3 in
                   let def_die_list = get_roll_die_list [] 2 in
                   draw_dice atk_die_list def_die_list;
                   attack_helper_2 atk_die_list def_die_list def atk

(* handles foritification *)
let rec fortify_terr st =
  print_endline (" \n Which territory would you like " ^
    "to fortify? Enter \"end\"" ^ "if you changed you mind.");
  let to_terr = String.(read_line () |> trim |> lowercase_ascii) in
  if to_terr = "end"
    then []
  else
    ( print_endline "\n From which territory would you like to move the ta's?";
      let from_terr = String.(read_line () |> trim |> lowercase_ascii) in
      if from_terr = "end"
        then []
        else (print_endline "\n How many territories would you like to move?";
              let num = String.(read_line () |> trim |> lowercase_ascii) in
              if num = "end"
                then []
              else (let fnum = int_of_string num in
                    if List.mem to_terr (controlled_names (st.current_player))
                    && List.mem from_terr (controlled_names (st.current_player))
                    && fnum < get_tas !((List.assoc from_terr st.map).territory)
                     then [to_terr;from_terr;num]
                    else raise Illegal)))

(* returns [true] if the player can fortify, [false] otherwise *)
let can_fortify st =
  let terrs = controlled_territories st.current_player in
  if terrs = [] then false
  else List.exists (fun t -> (get_tas t) > 1) terrs

(* helper function for reinfoce *)
let rec help_reinforce st n =
    match n with
    | 0 ->  draw_ta_num n ; print_endline "Done reinforcing!"
    | i ->
      let () = draw_ta_num n in
      if is_ai (player_name st.current_player) then
          let terr =  ai_reinforce st.current_player st.map in
          let () = update_territory terr 1 in
          let () = updatemap st in
          help_reinforce st (n-1)
      else
      try
      let () = draw_ta_num n in
      let () = print_endline "Name the territory you would like to reinforce" in
      let terr_to_reinforce = parsed (read_line()) in
      let () = print_endline ("Name the number of soldiers you would like to " ^
        "reinforce with") in
      let num_tas_to_reinforce = int_of_string (parsed (read_line())) in
      if List.mem_assoc terr_to_reinforce st.map
      then
        (if !((List.assoc terr_to_reinforce st.map).occupant)
          = player_name st.current_player && (num_tas_to_reinforce)<=n
        then (update_territory
          (!((List.assoc terr_to_reinforce st.map).territory))
          num_tas_to_reinforce;
          updatemap st;
          help_reinforce st (n-num_tas_to_reinforce))
        else ( print_endline "\n\t--- Invalid Territory ---";
         help_reinforce st n))
      else (print_endline "\n\t--- Invalid Territory ---";
         help_reinforce st n)
      with
      | _ -> (print_endline "\n\t--- Invalid Num of TAs ---";
              help_reinforce st n)

(* handles reinforce *)
let rec reinforce st =
  print_endline (player_name (st.current_player));
  let newnumtas = get_new_tas st in
  print_endline (string_of_int newnumtas);
  if is_ai (player_name st.current_player)
    then (help_reinforce st newnumtas)
  else (
    let () = print_endline ("You have " ^ string_of_int newnumtas ^
     " tas to reinforce with.") in
      help_reinforce st newnumtas )
(* handles attack *)
and attack st =
    if is_ai (player_name st.current_player)
    then(
      print_endline "attacking...";
    match ai_attack st.current_player st.map with
    |Some(atk,def) ->
            if adjacent atk def && can_attack st.map atk
            then ( attack_compare_results atk def;
            if get_tas def = 0
            then
              (let def_player = find_player st
                !((List.assoc (get_name def) (st.map)).occupant) in
              let () = remove_territory def_player def in
              let () = add_territory st.current_player def in
              let () = update_map st.map def (player_name st.current_player) in
              let () = updatemap st in
              let num = get_tas atk in
              let () = update_territory atk (-(num/2)) in
              let () = update_territory def (num/2) in
              let () = updatemap st in
              let () = update_cards st.current_player [get_card st.deck] in
              print_endline "Won over the territory";
              (match is_anyone_defeated st with
              | [] ->  attack st
              | [h] -> update_defeated st h;
              remove_player st (player_name h);
              update_cards st.current_player (cards_in_hand def_player);
              ))
            else (attack st)
          )else (attack st)
    |None -> print_endline "Done attacking"
  )
  else
  (let () = print_endline "Do you want to attack? Type yes or no" in
    match parsed (read_line()) with
    |"yes" ->
      let () = print_endline "Attack from" in
      let attacking_from = parsed (read_line()) in
      let () = print_endline "Attacking territory" in
      let defending_from =  parsed (read_line()) in
      if List.mem_assoc attacking_from st.map
        && List.mem_assoc defending_from st.map
      then (let atk = List.assoc attacking_from st.map in
            let def = List.assoc defending_from st.map in
            if adjacent !(atk.territory) !(def.territory)
              && can_attack st.map !(atk.territory)
              && !(atk.occupant) <> !(def.occupant)
            then (attack_compare_results !(atk.territory) !(def.territory);
                  updatemap st;
            if get_tas !(def.territory) = 0 then
              (let def_player = find_player st !(def.occupant) in
              let () = remove_territory def_player !(def.territory) in
              let () = add_territory st.current_player !(def.territory) in
              let () = update_map st.map !(def.territory)
                       (player_name st.current_player) in
              let () = conquered_a_terr !(atk.territory) !(def.territory) in
              let () = updatemap st in
              let () = update_cards st.current_player [get_card st.deck] in
              (match is_anyone_defeated st with
              | [] ->  attack st
              | [h] -> update_defeated st h;
              remove_player st (player_name h);
              update_cards st.current_player (cards_in_hand def_player);
              updatemap st
              )
            )else (attack st))
          else (print_endline "Can't attack from that territory"; attack st))
        else (print_endline "One of your territories was invalid; try again";
              attack st)
    |"no" -> print_endline "Done attacking"
    |_  -> print_endline "Please type yes or no"; attack st)
(* handles fortify *)
and fortify st =
if is_ai (player_name st.current_player) then
  (match ai_fortify st.current_player st.map with
    |None ->
      update_player_list st;
      update_curr_player st;
      updatemap st
    |Some (to_terr,from_terr,i) ->
      update_territory to_terr i;
      update_territory from_terr (-i);
      update_player_list st;
      update_curr_player st;
      updatemap st)
else (
  if not (can_fortify st) then(
    print_endline ("You can't fortify right now. " ^
           "Press any key to end your turn.");
    update_player_list st; update_curr_player st;
    ignore (read_line ()))
else (
  print_endline " Would you like to fortify a territory? (Yes\\No)\n";
  let input = String.(read_line () |> trim |> lowercase_ascii) in
  if input = "yes"
    then (
            try
              let cmd = fortify_terr st in
              if cmd = []
                then  (update_player_list st; update_curr_player st)
              else (
                let to_terr = !((List.assoc (List.nth cmd 0)
                   (st.map)).territory) in
                let from_terr = !((List.assoc (List.nth cmd 1)
                   (st.map)).territory) in
                let amnt = int_of_string (List.nth cmd 2) in
                update_territory to_terr amnt;
                update_territory from_terr (-amnt);
                update_player_list st;
                update_curr_player st)
            with
            | _ -> print_endline "\n Invalid input"; fortify st)
  else (if input = "no"
    then ( update_player_list st;
           update_curr_player st)
    else
      ( print_endline " Invalid input.";
        fortify st ))))
(* the main repl *)
and rungame st =
  try
    highlight_cp st;
    draw_cards (cards_in_hand (st.current_player));
    highlight_reinforce();
    reinforce st;
    highlight_attack ();
    attack st;
    highlight_fortify ();
    fortify st;
    rungame st
  with
  |Illegal -> print_endline " Invalid entry"; rungame st

(* handles the initial occupy process *)
let rec init_occupy st n =
    match n with
    | 0 -> print_endline ("\nDone placing your preliminary TAS\n"); rungame st
    | i -> updatemap st;
        highlight_cp st;
        if is_ai (player_name st.current_player)
        then(
          if (i > 70) then (
            let occ_terr = select_terrs st.current_player st.map
               (get_empty_terrs st.map) in
            let ai_name = player_name (st.current_player) in
            occupy occ_terr st.current_player st;
            print_endline ("\n" ^ ai_name ^ " occupied " ^ occ_terr);
            init_occupy st (n-1) )
          else (
            let rein_terr = get_name (ai_reinforce st.current_player st.map) in
            let ai_name = player_name (st.current_player) in
            occupy rein_terr st.current_player st;
            print_endline ("\n" ^ ai_name ^ " reinforced " ^ rein_terr);
            init_occupy st (n-1) )
          )
        else
          try
          (
          (if i > 70
            then (print_endline ("\n" ^ player_name (st.current_player) ^
                    ", name the territory you would like to occupy."))
            else (print_endline ("\n" ^ player_name (st.current_player) ^
                    ", name the territory you would like to reinforce.")));
          print_string  "> ";
          let t = String.(read_line () |> lowercase_ascii |> trim ) in
          if t = "quit"
            then failwith ("quit")
          else ( if t = "help"
            then (show_help ();
                 init_occupy st n)
            else
            (
                if t = "available"
                  then (print_endline "";
                       (List.iter (fun s -> print_endline s)
                        (List.map (fun terr -> get_name terr )
                          (get_empty_terrs st.map)));
                       print_endline ""; init_occupy st n)
                  else
                    (occupy t st.current_player st;
                    init_occupy st (n-1))
            )
          ))
          with
          | Illegal -> init_occupy st n
          | Not_found -> print_endline "\n\t--- Invalid Territority ---";
                         init_occupy st n

(* starts the game *)
let main num =
  Random.self_init ();
  let names =  get_names num num [] in
  let name_list = player_rolls_die names in
  let ordered_list = players_in_order name_list in
  print_order ordered_list;
  let st = init_state ordered_list in
  draw_initial_graph st;
  let initocc = init_occupy st 100 in
  ()