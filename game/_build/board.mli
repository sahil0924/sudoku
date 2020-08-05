(** The abstract type of values representing Sudoku games *)
type t

(** The type representing a single box *)
type box = 
  | Guess of int
  | Final of int
  | Empty

(** The type of values representing board objects *)
type board = (string * box) list

(** The type representing a result of solving *)
type result = 
  | MultSol
  | OneSol of board
  | NoSol

(** Thrown when a key is invalid *)
exception InvalidKey

(** Thrown when a value is invalid *)
exception InvalidValue

(** [make_game j] is the game that [j] represents.
    Requires: [j] is a valid JSON adventure representation. *)
val make_game : Yojson.Basic.t -> t

(** [solution b] is the solution to the board [b] *)
val solution :  board -> result

(** [get_box key g b] is the box at the given key of the puzzle board if b, 
    otherwise is the box at the given key of the answer board. For example,
    [get_box "a4" g true] returns the key at row a, column 4 of the puzzle 
    board. 
    Raises [InvalidKey] if the key is ill typed.*)
val get_box : string -> board -> box

(** [set_box key g new] is [g] with the box at the given key of the puzzle board
    switched to [Guess new]. For example, [set_box "a4" game 4] returns game with
    the box at row a, column 4 of the puzzle board now Guess 4.
    Raises [InvalidKey] if the key is ill typed*)
val set_box : string -> int  -> board -> board

(** [get_puzzle t] Returns t.puzzle, the current puzzle.*)
val get_puzzle : t -> board

(** [get_answer t] Returns t.answer, the current puzzle's answer key.*)
val get_answer : t -> board

(** [check puzzle answer] Returns the new state of the puzzle after comparing
    user's inputs to answer key.*)
val check : board -> board -> board

(** [winner puzzle answer] retunrs true if the puzzle is completed and checked
    with the answer key.*)
val winner : board -> board -> bool

(** [numify k] is the coordinate representation of [k]. For example, 
    [numify "a4" is (1, 4)] *)
val numify : string -> int * int

(** [valid i k bd] is whether or not the number [i] is allowed to go in 
    position [k] in board [bd] *)
val valid : int -> string -> board -> bool

(** [clear puzzle] returns the puzzle after it has removed all guesses. *)
val clear : board -> board

(** [hint puzzle answer] returns a board after the hint was applied. *)
val hint : board -> board -> board

(** [to_board lst] is a board representation of [lst] 
    Requires: [lst] contains 9 elements, each containing 9 elements*)
val to_board: (int list) list -> board

(** [make_game_from_gen board diff] generates a game with board [board] and 
    difficult [diff]*)
val make_game_from_gen : board -> Command.difficulty -> t