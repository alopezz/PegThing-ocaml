(* General sequence utilities *)

module Seqq = struct
  include Seq
  let rec take n seq = match n with
    | 0 -> []
    | x when x < 0 -> []
    | _ -> match seq with
           | Seq.Cons(value, rest) -> value :: (take (n - 1) (rest ()))
           | Seq.Nil -> []

  let rec take_while pred seq = match seq with
    | Seq.Nil -> []
    | Seq.Cons(value, rest) -> match (pred value) with
                               | true -> value :: (take_while pred (rest ()))
                               | false -> []
end

let last lst = match lst with
  | [] -> None
  | _ -> Some (List.hd (List.rev lst))

(* A lazy sequence of triangular numbers *)
let tri =
  let rec tri_inner accum n =
    let new_sum = accum + n in
    Seq.Cons (new_sum, (fun () -> (tri_inner new_sum (n + 1))))
  in (tri_inner 0 1)

let is_triangular x =
  match (last (Seqq.take_while (fun y -> y <= x) tri)) with
  | Some y -> x = y
  | None -> false

let row_tri n =
  match (last (Seqq.take n tri)) with
  | Some x -> x
  | None -> 0

let row_num pos =
  1 + List.length (Seqq.take_while (fun y -> y < pos) tri)

type peg = {is_pegged: bool; connections: (int * int) list}

(* Later: make a functor to create boards of arbitrary sizes *)

module IntMap = Map.Make(Int)

type board = peg IntMap.t

(* Form a mutual connection between two positions.
   If the position doesn't exist in the board, it will
   be created with is_pegged = true *)
let connect board max_pos pos neighbor destination =
  if destination <= max_pos
  then List.fold_left
         (fun new_board (p1, p2) ->
           let my_peg = match IntMap.find_opt p1 new_board with
             | Some(p) -> p
             | None -> {is_pegged=true; connections=[]}
           in
           IntMap.add p1 {my_peg with
               connections=(p2, neighbor)::my_peg.connections}
             new_board)
         board
         [(pos, destination); (destination, pos)]
  else board


let connect_right max_pos pos board =
  let neighbor = pos + 1 in
  let destination = neighbor + 1 in
  if (is_triangular neighbor) || (is_triangular pos)
  then board
  else (connect board max_pos pos neighbor destination)

let connect_down_left max_pos pos board =
  let row = row_num pos in
  let neighbor = row + pos in
  let destination = row + neighbor + 1 in
  connect board max_pos pos neighbor destination

let connect_down_right max_pos pos board =
  let row = row_num pos in
  let neighbor = row + pos + 1 in
  let destination = row + neighbor + 2 in
  connect board max_pos pos neighbor destination

(* Peg the position and perform connections *)
let add_pos board max_pos pos =
  let board = match IntMap.find_opt pos board with
    | Some _ -> board
    | None -> IntMap.add pos {is_pegged=true; connections=[]} board
  in
  List.fold_left
    (fun new_board create_connections ->
      create_connections max_pos pos new_board)
    board
    [connect_right; connect_down_left; connect_down_right]

(* Create a new board with the given number of rows *)
let new_board rows =
  let initial_board = IntMap.empty in
  let max_pos = (row_tri rows) in
  let board = List.fold_left
                (fun board pos -> add_pos board max_pos pos)
                initial_board
                (List.init max_pos ((+) 1)) in
  board

(* ** Moving pegs ** *)
(* Does the position have a peg in it? *)
let is_pegged pos board =
  (IntMap.find pos board).is_pegged

(** How many pegs (pegged positions) are in the board? *)
let num_of_pegs board =
  IntMap.filter
    (fun pos _ -> is_pegged pos board)
    board |> IntMap.cardinal

let set_peg board pos state =
  let the_peg = (IntMap.find pos board) in
  IntMap.add pos {the_peg with is_pegged=state} board

let remove_peg pos board = set_peg board pos false

let place_peg pos board = set_peg board pos true

let move_peg p1 p2 board =
  remove_peg p1 board |> place_peg p2

let connections_of pos board =
  (IntMap.find pos board).connections


let valid_moves pos board =
  List.filter (fun (destination, jumped) ->
      not (is_pegged destination board) && (is_pegged jumped board))
    (connections_of pos board)

(** Return an Option with the jumped pegged when the move is valid *)
let valid_move p1 p2 board =
  valid_moves p1 board |> List.assoc_opt p2

(** Move peg from p1 to p2, removing the jumped peg *)
let make_move p1 p2 board =
  match valid_move p1 p2 board with
  | Some jumped -> Some (move_peg p1 p2 board |> remove_peg jumped)
  | None -> None

(** Determine if there are available moves on the board (to know if
   the game is over) *)
let can_move board =
  IntMap.exists
    (fun pos _ ->
      is_pegged pos board && List.length (valid_moves pos board) > 0)
    board

(** ***Board representation*** **)
let letter_for_pos pos = Char.chr (pos + 96)

(* TODO implement colorization *)
let render_pos board pos =
  Printf.sprintf "%c%s"
    (letter_for_pos pos)
    (if is_pegged pos board then "0" else "-")

(* Return all positions for given row *)
let row_positions row_num =
  let range = List.init (row_tri row_num) ((+) 1) in
  let prev_tri = row_tri (row_num - 1) in
  List.filter ((<) prev_tri) range

let pos_chars = 3

let row_padding row_num rows =
  let pad_length = (rows - row_num) * pos_chars / 2 in
  List.init pad_length (fun _ -> " ") |> String.concat ""

let num_rows board =
  let max a b = if a >= b then a else b in
  row_num (IntMap.fold (fun k _ m -> max k m) board 0)

let render_row board row_num =
  row_padding row_num (num_rows board) ^ (
    String.concat " "
      (List.map (render_pos board) (row_positions row_num))
  )

let render_board board =
  let n_rows = (num_rows board) in
  (String.concat "\n"
     (List.init n_rows ((+) 1) |> List.map (render_row board))
   ^ "\n")

(** Player interaction **)
let pos_of_letter letter =
  Char.code letter - 96
