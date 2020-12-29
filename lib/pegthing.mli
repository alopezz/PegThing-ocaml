type board

val new_board : int -> board

val render_board: board -> string

val num_of_pegs: board -> int

val pos_of_letter: char -> int

val make_move: int -> int -> board -> board option

val remove_peg: int -> board -> board

val can_move: board -> bool
