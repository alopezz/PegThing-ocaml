type board

(** Create a new board with a given number of rows *)
val new_board : int -> board

(** Return a string representation of the board *)
val render_board: board -> string

(** Return the number of pegs on a board *)
val num_of_pegs: board -> int

(** Attempt to make a move between two positions identified with letters *)
val make_move: char -> char -> board -> board option

(** Remove peg at the position identified by the given letter *)
val remove_peg: char -> board -> board

(** Return whether there are available moves on the board *)
val can_move: board -> bool
