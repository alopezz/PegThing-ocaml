
let get_input ?(default="") () =
  match read_line () with
      | "" -> default
      | a -> a

let print_board board = print_string (Pegthing.render_board board)

(** These mutually recursive functions act as the loop that keeps
   prompting the player for input (moves, etc.) and shows the board
   state *)
let rec start_game () =
  let n_rows = prompt_rows () in
  let board = Pegthing.new_board n_rows |> prompt_empty_peg in
  if Pegthing.can_move board then prompt_move board
  else begin
      print_endline "This board starts without valid moves! Let's try again";
      start_game ()
    end

and prompt_empty_peg board =
  try begin
      print_endline "Here's your board:";
      print_board board;
      print_endline "Remove which peg? [e]";
      let position = Scanf.sscanf (get_input ~default:"e" ()) "%c" Fun.id in
      Pegthing.remove_peg position board
    end with
  | End_of_file | Not_found -> prompt_empty_peg board

and prompt_rows () =
  print_endline "How many rows? [5]";
  let prompt_again () =
    print_endline "Invalid number of rows";
    prompt_rows ()
  in
  let rows =
    try Scanf.sscanf (get_input ~default:"5" ()) "%d" Fun.id with
    | Scanf.Scan_failure _ -> prompt_again ()
  in
  if rows > 0 then rows
  else prompt_again ()

and prompt_move board =
    print_endline "\nHere is your board:";
    print_board board;
    print_newline ();
    print_endline "Move from where to where? Enter two letters:";
    try begin
        let p1, p2 = Scanf.sscanf (get_input ()) "%c%c"
                       (fun a b -> (a, b))
        in
        match Pegthing.make_move p1 p2 board with
        | Some new_board -> (user_entered_valid_move new_board)
        | None -> (user_entered_invalid_move board)
      end with
    | End_of_file | Not_found -> (user_entered_invalid_move board)


and user_entered_valid_move board =
  if Pegthing.can_move board
  then prompt_move board
  else game_over board

and user_entered_invalid_move board =
  print_endline "\nThat was an invalid move!";
  prompt_move board

and game_over board =
  Printf.printf "Game over! You had %d pegs left:\n"
    (Pegthing.num_of_pegs board);
  print_board board;
  print_endline "Play again? y/n [y]";
  if (get_input ~default:"y" ()) = "y"
  then start_game ()
  else print_endline "Bye!"

let () = start_game ()
