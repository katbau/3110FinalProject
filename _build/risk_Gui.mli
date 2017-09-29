open Graphics
open Camlimages
open Risk_map
open Player
open Territory
open Card
open Deck

type state = {
  mutable current_player: player;
  mutable players : player list;
  mutable defeated : (player * bool) list;
  initial_order : (player * int) list;
  map : map;
  mutable redeem_num : int;
  deck : deck;
}

val draw_initial_graph : state -> unit

val draw_ta_num : int -> unit

val paintmap : state -> map_terr list ->  unit

val highlight_reinforce : unit -> unit

val highlight_attack : unit -> unit

val highlight_fortify : unit -> unit

val draw_dice : int list -> int list -> unit

val draw_cards : card list -> unit

val highlight_cp : state -> unit

val updatemap : state -> unit
