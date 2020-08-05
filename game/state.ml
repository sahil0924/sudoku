
open Sys
open Generator
open Board
open Command

type t = {
  game: Board.board;
  moves : int;
  hint : bool;
  time : float;
}

let init_state b = {
  game = get_puzzle b;
  moves = 0;
  hint = false;
  time = Sys.time ();
}

type result = 
  | Legal of t 
  | Illegal
  | Winner
  | NoHint
  | Help

let get_game t = t.game

let get_moves t = t.moves

let get_score t = 
  let elapsed_time = (Sys.time ()) -. t.time in 
  ((10. -. elapsed_time) +. 1.) *. 
  (1./.((float_of_int t.moves) +. 1.)) |> int_of_float;;

let move (key : string) (v : int) (st : t) = 
  match (get_box key st.game) with
  | Final _ -> Illegal
  | _ -> Legal {game = set_box key v st.game; moves = st.moves + 1;
                time = st.time; hint = st.hint}

let generate_state diff = 
  let b = create_generated_board initial_gen diff in 
  let game = make_game_from_gen b diff in 
  Legal (init_state game);;


let clear_state state = Legal {
    game = clear state.game;
    moves = state.moves;
    hint = state.hint;
    time = state.time;
  }

(** [hint_state_helper board state] returns a new state after the hint has 
    been applied *)
let hint_state_helper board state = Legal {
    game = hint state.game (get_answer board);
    moves = state.moves;
    hint = true;
    time = state.time;
  }

let hint_state board state = 
  if state.hint = true then NoHint else hint_state_helper board state

let check_state board state = 
  let new_state = check state.game (get_answer board) in 
  if winner (get_answer board) (new_state) then Winner else
    Legal {game = new_state; moves = state.moves; hint = state.hint; 
           time = state.time}

let help_state state = Help


