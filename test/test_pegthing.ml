open Pegthing
open OUnit2

let test_5_rows_board_has_15_pegs _ =
  assert_equal (new_board 5 |> num_of_pegs) 15

let test_4_rows_board_has_11_pegs _ =
  assert_equal (new_board 4 |> num_of_pegs) 10

let removing_a_peg _ =
  assert_equal (new_board 5 |> remove_peg 'e' |> num_of_pegs) 14

let full_board_has_no_moves _ =
  assert_equal (new_board 5 |> can_move) false

let test_can_move _ =
  assert_equal (new_board 5 |> remove_peg 'e' |> can_move) true

let test_invalid_move_return_None _ =
  assert_equal (new_board 5 |> remove_peg 'e' |> make_move 'a' 'c') None

let test_valid_move _ =
  let starting_board = new_board 5 |> remove_peg 'e' in
  assert_equal (make_move 'l' 'e' starting_board)
    (Some (new_board 5 |> remove_peg 'h' |> remove_peg 'l'))

let suite =
  "suite">:::
    ["test 5 rows board has 15 pegs">:: test_5_rows_board_has_15_pegs;
     "test 4 rows board has 11 pegs">:: test_4_rows_board_has_11_pegs;
     "remove a peg">:: removing_a_peg;
     "full board has no moves">:: full_board_has_no_moves;
     "can move">:: test_can_move;
     "invalid move returns None">:: test_invalid_move_return_None;
     "valid move">:: test_valid_move]

let () =
  run_test_tt_main suite


