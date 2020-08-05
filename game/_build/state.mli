(** The abstract type of values representing state objects *)
type t

(** The type result is the result of the state after executing a command. *)
type result = 
  | Legal of t 
  | Illegal
  | Winner
  | NoHint
  | Help

(** [init_state b] takes in a Board.t object and creates a state object.
    starting represents the starting board. 
    current represents the current board. 
    valid represents the string list of valid keys. 
    moves represents the number of moves made. *)
val init_state : Board.t -> t

(** [move b key v] takes in a Board.t object, string key, int value, and returns
    an object of type result.  If the key input is not valid, it will return 
    Illegal.  Otherwise, it will use it's helpers to create a new Board.t object and
    update the respective state object.  It will return Legal of State.t*)
val move : string -> int -> t -> result

(** [check_state board state] compares the current state of the user's inputs
    and updates the integers to final if they are correct. *)
val check_state : Board.t -> t -> result

(** [get_score state] Computes the current score of the game*)
val get_score : t -> int

(** [get_game t] Returns the current puzzle.*)
val get_game : t -> Board.board

(** [get_game t] Returns the currents moves*)
val get_moves : t -> int

(**[generate_stae diff state] takes in the difficulty [diff] and creates 
   a randomly generated corresponding board object.  It then creates initializes a
   state with the board.
*)
val generate_state : Command.difficulty -> result

(** [clear_state puzzle] clears the current game board by removing all 
    guesses. *)
val clear_state : t -> result

(** [hint_state board puzzle] adds a hint if possible or returns NoHint if a 
    hint was already applied. *)
val hint_state : Board.t -> t -> result

(** [help_state state] creates a type Legal of state *)
val help_state : t -> result
