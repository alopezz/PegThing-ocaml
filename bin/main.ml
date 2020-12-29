
let get_input ?(default="") ()=
  Scanf.scanf "%s\n" (fun input ->
      match String.trim(input) with
      | "" -> default
      | a -> a)

let print_board board = print_string (Pegthing.render_board board)

(** Announce the game is over and prompt to play again *)
let rec start_game n_rows =
  prompt_move (Pegthing.new_board n_rows |> Pegthing.remove_peg 4)

and prompt_move board =
  print_string "\nHere is your board:";
  print_newline ();
  print_board board;
  print_newline ();
  print_string "Move from where to where? Enter two letters:";
  print_newline ();
  let p1, p2 = Scanf.sscanf (get_input ()) "%c%c"
                 (fun a b -> (Pegthing.pos_of_letter a,
                              Pegthing.pos_of_letter b)) in
  match Pegthing.make_move p1 p2 board with
  | Some new_board -> (user_entered_valid_move new_board)
  | None -> (user_entered_invalid_move board)

and user_entered_valid_move board =
  if Pegthing.can_move board
  then prompt_move board
  else game_over board

and user_entered_invalid_move board =
  print_string "\nThat was an invalid move!\n";
  prompt_move board

and game_over board =
  Printf.printf "Game over! You had %d pegs left:\n"
    (Pegthing.num_of_pegs board);
  print_board board;
  print_string "Play again? y/n [y]";
  print_newline ();
  if (get_input ~default:"y" ()) = "y"
  then start_game 5
  else print_string "Bye!\n"



let () =
  start_game 5
