(* gets the number of human players to initialize the game *)
let rec get_players () =
    print_endline "\nPlease enter the number of human players.";
    print_string  "> ";
    let human_number = read_line () in
    let correct_format = String.lowercase_ascii (String.trim (human_number)) in
    try
        if correct_format = "quit"
            then print_endline "End"
        else(
            if (Pervasives.int_of_string correct_format )<= 4 && (Pervasives.int_of_string correct_format) > 0
            then (Game.main (Pervasives.int_of_string human_number))
            else print_endline "\n\n --- You can only have between 1 and 4 human players ---"; get_players () )
    with
    | Failure ("int_of_string") -> print_endline "\n\n --- Invalid Entry ---"; get_players ()
    | Failure ("quit") -> print_endline "End"

(* starts the game *)
let () =
   ANSITerminal.(print_string [red]
    "\n\n\tWelcome to Occupy Cornell.\n");
    get_players ()







