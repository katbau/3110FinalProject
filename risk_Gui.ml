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
  mutable redeem_num: int;
  deck : deck;
}

(*screen width and height parameters*)
let screen_width = 800
let screen_height = 600

(*set base of image and risk map*)
let initialx = 0
let initialy = 0


(*associated list of territory and its coordinates*)
let territory_map =
  [("keeton house",[|(42,177);(82,194);(88,223);(56,230);(41,217);(42,177)|]);
   ("bethe house",[|(97,208);(136,200);(139,210);(115,243);(81,243);(81,230);
                    (99,231);(97,208)|]);
   ("rose house",[|(75,254);(94,256);(118,266);(118,286);(88,289);(87,280);
                  (83,273);(72,268);(75,254)|]);
   ("becker house",[|(45,270);(62,270);(97,297);(90,307);(76,298);(72,308);
                    (53,300);(54,285);(42,285);(45,270)|]);
   ("cook house",[|(48,318);(56,323);(63,313);(73,313);(74,327);(85,329);
                  (80,344);(36,336);(48,318)|]);
   ("uris hall",[|(351,246);(350,218);(381,221);(380,247);(351,246)|]);
   ("psb",[|(366,394);(366,378);(358,375);(359,359);(405,357);(405,397);
            (366,394)|]);
   ("statler hall",[|(354,177);(358,129);(393,123);(393,162);(385,162);
                    (385,177);(354,177)|]);
   ("barton hall",[|(393,191);(460,182);(460,142);(393,142);(393,191)|]);
   ("ives hall",[|(396,243);(395,194);(442,194);(441,249);(396,243)|]);
   ("bailey hall",[|(438,359);(441,337);(446,328);(461,328);(466,340);
                  (466,359);(438,359)|]);
   ("mudd hall",[|(483,242);(483,223);(539,223);(539,243);(483,242)|]);
   ("kennedy hall",[|(473,334);(473,263);(505,263);(505,302);(490,302);
                    (490,334);(473,334)|]);
   ("teagle hall",[|(473,166);(472,135);(520,135);(521,167);(473,166)|]);
   ("comstock hall",[|(474,212);(474,172);(497,172);(498,212);(474,212)|]);
   ("duffield hall",[|(337,110);(339,45);(386,45);(386,110);(337,110)|]);
   ("gates hall",[|(400,114);(438,118);(438,105);(400,99);(400,114)|]);
   ("thurston hall",[|(287,60);(332,63);(336,37);(287,28);(287,60)|]);
   ("hollister hall",[|(250,60);(273,58);(277,24);(259,24);(250,60)|]);
   ("carpenter hall",[|(254,97);(278,98);(280,68);(259,68);(254,97)|]);
   ("goldwin smith hall",[|(303,371);(331,371);(331,306);(303,306);(303,371)|]);
   ("mcgraw hall",[|(226,344);(245,344);(245,383);(226,383);(226,344)|]);
   ("olin library",[|(259,290);(259,255);(310,255);(310,290);(259,290)|]);
   ("uris library",[|(223,279);(222,253);(258,250);(255,280);(226,281);
                    (223,279)|]);
   ("lincoln hall",[|(301,381);(335,381);(335,418);(301,418);(301,381)|]);
   ("willard straight hall",[|(209,215);(244,215);(244,177);(209,177);
                              (209,215)|]);
   ("olin hall",[|(254,170);(250,127);(299,127);(299,144);(281,144);(279,173);
                  (254,170)|]);
   ("sage hall",[|(298,145);(340,145);(340,174);(298,174);(298,145)|]);
   ("cornell store",[|(254,195);(312,192);(311,215);(297,229);(275,225);
                      (250,227);(252,214);(259,208);(249,208);(254,195)|]);
   ("gannett",[|(212,165);(220,135);(244,134);(245,168);(212,165)|])]

(*associated list of territory and its image*)
let territory_img = [("keeton house","images/keeton.png");
                    ("bethe house","images/bethe.png");
                    ("rose house","images/rose.png");
                    ("becker house","images/becker.png");
                    ("cook house","images/cook.png");
                    ("uris hall","images/urishall.png");
                    ("psb","images/psb.png");
                    ("statler hall","images/statler.png");
                    ("barton hall","images/barton.png");
                    ("ives hall","images/ives.png");
                    ("bailey hall","images/bailey.png");
                    ("mudd hall","images/mudd.png");
                    ("kennedy hall","images/kennedy.png");
                    ("teagle hall","images/teagle.png");
                    ("comstock hall","images/comstock.png");
                    ("duffield hall","images/duffield.png");
                    ("gates hall","images/gates.png");
                    ("thurston hall","images/thurston.png");
                    ("hollister hall","images/hollister.png");
                    ("carpenter hall","images/carpenter.png");
                    ("goldwin smith hall","images/goldwinsmith.png");
                    ("mcgraw hall","images/mcgraw.png");
                    ("olin library","images/olin.png");
                    ("uris library","images/urislibrary.png");
                    ("lincoln hall","images/lincoln.png");
                    ("willard straight hall","images/willardstraight.png");
                    ("olin hall","images/olinhall.png");
                    ("sage hall","images/sage.png");
                    ("cornell store","images/cornellstore.png");
                    ("gannett","images/gannett.png")]

(*where the image should show up*)
let territory_img_dimensions =
                    [("keeton house",(100,104));
                    ("bethe house",(100,171));
                    ("rose house",(100,171));
                    ("becker house",(100,171));
                    ("cook house",(100,171));
                    ("uris hall",(100,88));
                    ("psb",(100,88));
                    ("statler hall",(100,110));
                    ("barton hall",(100,177));
                    ("ives hall",(100,105));
                    ("bailey hall",(100,90));
                    ("mudd hall",(100,123));
                    ("kennedy hall",(100,88));
                    ("teagle hall",(100,88));
                    ("comstock hall",(100,88));
                    ("duffield hall",(100,88));
                    ("gates hall",(100,152));
                    ("thurston hall",(100,88));
                    ("hollister hall",(100,88));
                    ("carpenter hall",(100,125));
                    ("goldwin smith hall",(100,105));
                    ("mcgraw hall",(100,104));
                    ("olin library",(100,88));
                    ("uris library",(100,125));
                    ("lincoln hall",(100,88));
                    ("willard straight hall",(100,88));
                    ("olin hall",(100,88));
                    ("sage hall",(100,88));
                    ("cornell store",(100,105));
                    ("gannett",(100,158))]

(*where ta number is showing up*)
let terrtalist =
                    [("keeton house",(57,200));
                    ("bethe house",(102,222));
                    ("rose house",(61,285));
                    ("becker house",(95,267));
                    ("cook house",(57,325));
                    ("uris hall",(234,259));
                    ("psb",(374,377));
                    ("statler hall",(366,137));
                    ("barton hall",(417,161));
                    ("ives hall",(402,216));
                    ("bailey hall",(444,345));
                    ("mudd hall",(494,229));
                    ("kennedy hall",(483,285));
                    ("teagle hall",(489,149));
                    ("comstock hall",(479,191));
                    ("duffield hall",(350,83));
                    ("gates hall",(409,106));
                    ("thurston hall",(310,32));
                    ("hollister hall",(263,48));
                    ("carpenter hall",(267,80));
                    ("goldwin smith hall",(311,335));
                    ("mcgraw hall",(233,357));
                    ("olin library",(273,263));
                    ("uris library",(363,229));
                    ("lincoln hall",(305,392));
                    ("willard straight hall",(219,187));
                    ("olin hall",(267,132));
                    ("sage hall",(320,151));
                    ("cornell store",(274,200));
                    ("gannett",(223,150))]


let num_coords =
  [(0,[(0,0,12,2);(10,2,2,24);(0,2,2,24);(2,24,10,2)]);
   (1,[(1,0,10,2);(5,2,2,22);(2,24,5,2)]);
   (2,[(0,0,12,2);(0,2,2,10);(0,12,12,2);(10,14,2,10);(0,24,12,2)]);
   (3,[(0,0,12,2);(10,2,2,10);(2,12,10,2);(10,14,2,10);(0,24,12,2)]);
   (4,[(10,0,2,26);(0,12,12,2);(0,14,2,12)]);
   (5,[(0,0,12,2);(10,2,2,10);(0,12,12,2);(0,14,2,10);(0,24,12,2)]);
   (6,[(0,0,12,2);(10,2,2,10);(0,12,12,2);(0,0,2,24);(0,24,12,2)]);
   (7,[(10,0,2,24);(0,24,12,2)]);
   (8,[(0,0,12,2);(10,2,2,24);(0,12,12,2);(0,0,2,24);(0,24,12,2)]);
   (9,[(10,0,2,24);(0,12,12,2);(0,14,2,10);(0,24,12,2)])]

(* function that draws large number at x' y'*)
let drawnum x' y' num =
  let coords = List.assoc num num_coords in
  let rec draw = function
    | [] -> ()
    | (x,y,w,h)::t -> fill_rect (x+x') (y+y') w h; draw t
  in draw coords

(*draws remaining number of tas*)
let draw_ta_num number =
  set_color red;
  fill_rect 659 453 60 34;
  set_color black;
  let num = if number > 1000 then 999 else number in
  let hnrd = num/100 in
  let tens = (num - hnrd * 100)/10 in
  let ones = num - hnrd*100 - tens*10 in
  (if hnrd = 0 then () else drawnum 661 455 hnrd);
  drawnum 683 455 tens;
  drawnum 705 455 ones

(* draws string at x y, doesnt change location afterwards*)
let draw_spec_string x y str =
    let xold = current_x () in
    let yold = current_y () in
    moveto x y;
    draw_string str;
    moveto xold yold

(*display that the player has no cards*)
let have_no_cards () =
  fill_rect 675 313 48 17;
  fill_rect 653 40 94 38;
  set_color black;
  draw_rect 652 39 96 40;
  draw_spec_string 658 60 "YOU DON'T HAVE";
  draw_spec_string 678 45 "ANY CARDS"

(*writes the players name in the player boxes*)
let rec write_player_names p_list counter =
  match p_list with
  | [] -> ()
  | h::t ->(let nm = if String.length (player_name h) > 19
              then String.(player_name h |> (fun s -> sub s 0 19) |> trim)
              else player_name h in
            let (w,l) = text_size nm in
            let start = (125 - w)/2 in
            (match counter with
            | 1 -> moveto (20+start) 557;
                   draw_string nm
            | 2 -> moveto (165+start) 557;
                   draw_string nm
            | 3 -> moveto (310+start) 557;
                   draw_string nm
            | 4 -> moveto (455+start) 557;
                   draw_string nm
            | _ -> () ); write_player_names t (counter + 1))


  (* highlights Reinforce*)
  let highlight_r = [(0,0,190,5);(0,5,5,90);(185,5,5,90);(0,95,190,5)]

  (* highlights Attack and Fortify *)
  let highlight_af = [(0,0,190,4);(0,4,4,56);(186,4,4,56);(0,56,190,4)]

  (*player turn highlight*)
  let highlight_pl = [(0,0,139,5);(0,44,139,5);(0,5,5,39);(134,5,5,39)]


(* [draw x y highlight] draws a highlighted box at x y as specified by highlight
 * draws highlighted boxes around current player and current game state*)
let rec draw x' y' = function
  | [] -> ()
  | (x,y,w,h)::t -> fill_rect (x+x') (y+y') w h; draw x' y' t

let highlight_reinforce () =
  set_color red;
  draw 605 353 highlight_af;
  draw 605 281 highlight_af;
  set_color white;
  draw 605 425 highlight_r;
  set_color black

let highlight_attack () =
  set_color red;
  draw 605 425 highlight_r;
  draw 605 281 highlight_af;
  set_color white;
  draw 605 353 highlight_af;
  set_color black

let highlight_fortify () =
  set_color red;
  draw 605 353 highlight_af;
  draw 605 425 highlight_r;
  set_color white;
  draw 605 281 highlight_af;
  set_color black



(*highlights the current player*)
let highlight_cp state =
  let player_num = List.assoc (state.current_player) (state.initial_order) in
  match player_num with
  | 1 -> set_color red;
         draw 158 538 highlight_pl;
         draw 303 538 highlight_pl;
         draw 448 538 highlight_pl;
         set_color white;
         draw  13 538 highlight_pl;
         set_color black
  | 2 -> set_color red;
         draw  13 538 highlight_pl;
         draw 303 538 highlight_pl;
         draw 448 538 highlight_pl;
         set_color white;
         draw 158 538 highlight_pl;
         set_color black
  | 3 -> set_color red;
         draw 158 538 highlight_pl;
         draw  13 538 highlight_pl;
         draw 448 538 highlight_pl;
         set_color white;
         draw 303 538 highlight_pl;
         set_color black
  | 4 -> set_color red;
         draw 158 538 highlight_pl;
         draw 303 538 highlight_pl;
         draw  13 538 highlight_pl;
         set_color white;
         draw 448 538 highlight_pl;
         set_color black
  | _ -> ()


  (*
  let cross_out_players () =
    set_line_width 3;
    match pl with
    | 1 -> moveto 21 546;
           lineto 144 579;
           moveto 21 579;
           lineto 144 546

    | 2 -> moveto 166 546;
           lineto 289 579;
           moveto 166 579;
           lineto 289 546

    | 3 -> moveto 311 546;
           lineto 434 579;
           moveto 311 579;
           lineto 434 546

    | 4 -> moveto 456 546;
           lineto 579 579;
           moveto 456 579;
           lineto 579 546
    | _ -> ();
    set_line_width 0
  *)


(*initial welcome text*)
let initialwriting str size font x0 y0 =
  moveto x0 y0;
  set_text_size 100;
  draw_string "WELCOME TO OCCUPY CORNELL"


(*function that loads the map of cornell*)
let loadinitialmap (map: string) =
  let cornellmap = Png.load_as_rgb24 map [] in
  Graphic_image.draw_image cornellmap initialx initialy


(*helper function that colors in said territory when conquered*)
let conqterrfill territory_map terr =
  let coords = List.assoc terr territory_map in
  fill_poly coords

(*function that makes a zoomed up picture of territory when typed into terminal*)
let zoomedimage territory_img_dimensions terr =
  let cornercoords = List.assoc terr territory_img_dimensions in
  let x' = fst cornercoords in
  let y' = snd cornercoords in
  (* match cornercoords with
  |[] -> ()
  |(h,t) ->
    let x'= h in
    let y'= t in *)
  let img = Png.load_as_rgb24 terr [] in
  Graphic_image.draw_image img x' y'

(* coordinates for die faces *)
let dice_dots =
    [(1,[(16,15)]);
     (2,[(5,26);(27,4)]);
     (3,[(16,15);(5,26);(27,4)]);
     (4,[(5,26);(27,4);(5,4);(27,26)]);
     (5,[(16,15);(5,26);(27,4);(5,4);(27,26)]);
     (6,[(5,26);(27,4);(5,4);(27,26);(5,15);(27,15)])]

(* draws a die of value [num] at (x,y)*)
let draw_die x y num =
  set_color white;
  fill_rect x y 36 36;
  set_color black;
  draw_rect (x-1) (y-1) 38 38;
  let patt = List.assoc num dice_dots in
  let rec draw_dot = function
  | [] -> ()
  | (a,b)::t -> fill_rect (x+a) (y+b) 6 6; draw_dot t
  in draw_dot patt

(* draws dice, given by int list [dice]*)
let draw_dice_helper y dice =
  match dice with
  | [] -> ()
  | d1::d2::d3::[] -> draw_die 620 y d1;
                      draw_die 680 y d2;
                      draw_die 740 y d3
  | d1::d2::[] -> draw_die 620 y d1;
                  draw_die 680 y d2;
                  set_color red;
                  fill_rect 739 (y-1) 38 38
  | d1::[] -> draw_die 620 y d1;
              set_color red;
              fill_rect 679 (y-1) 38 38;
              fill_rect 739 (y-1) 38 38
  | _ -> ()

(* draws the attackers and defenders dice. [attacker] and [defender]
 * are int lists sorted in increasing order.*)
let draw_dice attacker defender =
  set_color red;
  fill_rect 619 201 100 38;
  fill_rect 619 131 100 38;
  set_color black;
  draw_dice_helper 202 attacker;
  draw_dice_helper 132 defender

(*draws the cards on user interface*)
let draw_card x y card =
  draw_rect x y 30 45;
  set_color (rgb 255 204 153);
  fill_rect (x+1) (y+1) 28 43;
  set_color black;
  match card with
  | Card (n, Rank_I)   -> let w = if String.length n = 3 then 7 else 4 in
                          draw_spec_string (x+w) (y+25) n;
                          draw_spec_string (x+13) (y+7) "I"
  | Card (n, Rank_II)  -> let w = if String.length n = 3 then 7 else 4 in
                          draw_spec_string (x+w) (y+25) n;
                          draw_spec_string (x+10) (y+7) "II"
  | Card (n, Rank_III) -> let w = if String.length n = 3 then 7 else 4 in
                          draw_spec_string (x+w) (y+25) n;
                          draw_spec_string (x+7) (y+7) "III"

(**)
let adjust_card_lst lst =
  match lst with
  | a::b::c::d::e::f::tl -> [a;b;c;d;e;f]
  | other -> other

(**)
let draw_cards cards =
  let card_lst = adjust_card_lst cards in
  set_color red;
  fill_rect 649 9 132 105;
  set_color black;
  let rec helper x y = function
  | [] -> ()
  | c1::[] -> draw_card x y c1
  | c1::c2::[] -> draw_card x y c1;
                  draw_card (x+50) y c2
  | c1::c2::c3::tl -> draw_card x y c1;
                      draw_card (x+50) y c2;
                      draw_card (x+100) y c3;
                      helper x (y-55) tl
  in helper 650 65 card_lst


(*function that draws all the territories*)
let drawterr () =
  set_color black;
  (*MAKE KEETON*)
  draw_poly [|(42,177);(82,194);(88,223);(56,230);(41,217);(42,177)|];
  (*MAKE BETHE*)
  draw_poly [|(97,208);(136,200);(139,210);(115,243);(81,243);(81,230);
              (99,231);(97,208)|];
  (*ROSE*)
  draw_poly [|(75,254);(94,256);(118,266);(118,286);(88,289);(87,280);
              (83,273);(72,268);(75,254)|];
  (*BECKER*)
  draw_poly [|(45,270);(62,270);(97,297);(90,307);(76,298);(72,308);
              (53,300);(54,285);(42,285);(45,270)|];
  (*LINCOLN*)
  draw_poly [|(301,381);(335,381);(335,418);(301,418);(301,381)|];
  (*GOLDWIN SMITH*)
  draw_poly [|(303,371);(331,371);(331,306);(303,306);(303,371)|];
  (*OLIN LIBRARY*)
  draw_poly [|(259,290);(259,255);(310,255);(310,290);(259,290)|];
  (*URIS LIBRARY*)
  draw_poly [|(223,279);(222,253);(258,250);(255,280);(226,281);(223,279)|];
  (*MCGRAW HALL*)
  draw_poly [|(226,344);(245,344);(245,383);(226,383);(226,344)|];
  (*Gannet*)
  draw_poly [|(212,165);(220,135);(244,134);(245,168);(212,165)|];
  (*williard straight*)
  draw_poly [|(209,215);(244,215);(244,177);(209,177);(209,215)|];
  (*cornell store*)
  draw_poly [|(254,195);(312,192);(311,215);(297,229);(275,225);(250,227);
              (252,214);(259,208);(249,208);(254,195)|];
  (*olin hall*)
  draw_poly [|(254,170);(250,127);(299,127);(299,144);(281,144);(279,173);
              (254,170)|];
  (*sage*)
  draw_poly [|(298,145);(340,145);(340,174);(298,174);(298,145)|];
  (*COOK*)
  draw_poly [|(48,318);(56,323);(63,313);(73,313);(74,327);(85,329);
              (80,344);(36,336);(48,318)|];
  (*hollister*)
  draw_poly [|(250,60);(273,58);(277,24);(259,24);(250,60)|];
  (*carpenter*)
  draw_poly [|(254,97);(278,98);(280,68);(259,68);(254,97)|];
  (*duffield*)
  draw_poly [|(337,110);(339,45);(386,45);(386,110);(337,110)|];
  (*gates*)
  draw_poly [|(400,114);(438,118);(438,105);(400,99);(400,114)|];
  (*thurston*)
  draw_poly [|(287,60);(332,63);(336,37);(287,28);(287,60)|];
  (*PSB*)
  draw_poly [|(366,394);(366,378);(358,375);(359,359);(405,357);
              (405,397);(366,394)|];
  (*Uris Hall*)
  draw_poly [|(351,246);(350,218);(381,221);(380,247);(351,246)|];
  (*Statler*)
  draw_poly [|(354,177);(358,129);(393,123);(393,162);(385,162);(385,177);
              (354,177)|];
  (*Barton*)
  draw_poly [|(393,191);(460,182);(460,142);(393,142);(393,191)|];
  (*Ives*)
  draw_poly [|(396,243);(395,194);(442,194);(441,249);(396,243)|];
  (*bailey*)
  draw_poly [|(438,359);(441,337);(446,328);(461,328);(466,340);(466,359);
              (438,359)|];
  (*kennedy*)
  draw_poly [|(473,334);(473,263);(505,263);(505,302);(490,302);(490,334);
              (473,334)|];
  (*mudd*)
  draw_poly [|(483,242);(483,223);(539,223);(539,243);(483,242)|];
  (*comstock*)
  draw_poly [|(474,212);(474,172);(497,172);(498,212);(474,212)|];
  (*teagle hall*)
  draw_poly [|(473,166);(472,135);(520,135);(521,167);(473,166)|]


(*helper function to get occ of terr*)
let paintmap state terrlst =
  let playerlist = List.map (fun (p,i) -> ((player_name p),i) ) state.initial_order in
  let cl = [blue;green;cyan;magenta] in
  let assoccolorlst =
    match playerlist with
    | (p1,n1)::(p2,n2)::(p3,n3)::(p4,n4)::[] -> [(p1,List.nth cl (n1-1) );
        (p2,List.nth cl (n2-1));(p3,List.nth cl (n3-1));(p4, List.nth cl (n4-1))]
    | _ -> []
  in
  let rec paint = function
  |[]->()
  |h::t when !(h.occupant) <> "" -> set_color (List.assoc !(h.occupant) assoccolorlst);
           fill_poly (List.assoc (get_name !(h.territory)) territory_map);
           paint t
  |h::t when !(h.occupant) = "" ->  paint t
  in paint terrlst; set_color black

(*displayes the number of tas on the territory*)
let displayta state =
 (* failwith "unimplemented" *)
  set_color black;
  let assoc_list = List.fold_left (fun acc (x,y) -> (x, (get_tas (!(y.territory))))::acc) [] state.map in
  let rec draw_names lst =
    match lst with
    |[]->()
    |(h,t)::tl ->
    try
      let (x,y) = List.assoc h terrtalist in
      moveto x y;
      draw_string (string_of_int t);
      draw_names tl
    with
    _ -> ()
  in
  draw_names assoc_list


(*function that displays the map with drawn on coordinates and territories colored in*)
 let updatemap state =
  let map_img = Png.load_as_rgb24 "images/mapcornell3110.png" [] in
  Graphic_image.draw_image map_img 0 0;
  let cornell_logo = Png.load_as_rgb24 "cornelllogo1.png" [] in
  Graphic_image.draw_image cornell_logo 232 480;
  drawterr();
  let terrlist = snd (List.split state.map) in
  paintmap state terrlist;
  displayta state



(*open initial screen*)
let draw_initial_graph state =
  open_graph " 800x600";
  set_line_width 0;
  set_color (rgb 28 153 60);
  fill_rect 0 0 800 600;
  set_color black;
  draw_rect 601 0 199 600;
  draw_rect 0 477 600 123;
  (* colors top and side*)
  set_color red;
  fill_rect 601 0 199 600;
  fill_rect 0 477 600 123;
  (* makes player box outlines *)
  set_color black;
  draw_rect  19 544 127 37;
  draw_rect 164 544 127 37;
  draw_rect 309 544 127 37;
  draw_rect 454 544 127 37;
  draw_rect 619 544 162 37;
  draw_rect 618 543 164 39;
  draw_rect 0 0 600 476;
  draw_rect 0 0 800 600;
  (* makes player boxes *)
  set_color blue;
  fill_rect 20 545 125 35;
  set_color green;
  fill_rect 165 545 125 35;
  set_color cyan;
  fill_rect 310 545 125 35;
  set_color magenta;
  fill_rect 455 545 125 35;
  (* makes "Occupy Cornell" box *)
  set_color (rgb 215 215 215);
  fill_rect 620 545 160 35;
  (* draws player name and title *)
  set_color black;
  write_player_names (state.players) 1;
  moveto 658 555;
  draw_string "OCCUPY CORNELL";
  (* dividing lines*)
  draw_rect 606 275 186 0;
  draw_rect 606 347 186 0;
  draw_rect 606 419 186 0;
  draw_rect 606 119 186 0;
  (* colors game state boxes light gray*)
  set_color (rgb 215 215 215);
  fill_rect 669 497 60 17;
  fill_rect 678 385 42 17;
  (* no cards image *)
  have_no_cards ();
  set_color black;
  (* box borders *)
  draw_rect 668 496 62 19;
  draw_rect 677 384 44 19;
  draw_rect 674 312 50 19;
  draw_spec_string 673 499 "REINFORCE";
  draw_spec_string 682 387 "ATTACK";
  draw_spec_string 643 365 "Attack your enemies";
  draw_spec_string 679 315 "FORTIFY";
  draw_spec_string 628 293 "Fortify your territories";
  draw_spec_string 658 435 "REMAINING TA'S";
  draw_ta_num 0;
  (*Dice*)
  draw_spec_string 610 252 "ATTACKER";
  draw_spec_string 610 179 "DEFENDER";
  (*Cards*)
  draw_spec_string 610  98 "CARDS";
  let map_img = Png.load_as_rgb24 "images/mapcornell3110.png" [] in
  Graphic_image.draw_image map_img 0 0;
  let cornell_logo = Png.load_as_rgb24 "cornelllogo1.png" [] in
  Graphic_image.draw_image cornell_logo 232 480;
  drawterr();
  highlight_reinforce ();
  highlight_cp state

