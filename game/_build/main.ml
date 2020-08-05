open Yojson.Basic.Util
open Generator
open Board 
open State
open Command
open Format

(** The following are colors we will use in the board for each integer *)
let grey	s = "\027[1;30m" ^ s ^ "\027[0m"
let red	s = "\027[1;31m" ^ s ^ "\027[0m"
let green	s = "\027[1;32m" ^ s ^ "\027[0m"
let yellow	s = "\027[1;33m" ^ s ^ "\027[0m"
let blue s = "\027[1;34m" ^ s ^ "\027[0m"
let magenta	s = "\027[1;35m" ^ s ^ "\027[0m"
let cyan	s = "\027[1;36m" ^ s ^ "\027[0m"
let white	s = "\027[1;37m" ^ s ^ "\027[0m"

(** [get_num board s] returns the digit that is currently in the key [s] of the
    sudoku board [board]
    Requires:
      [s] is a valid sudoku key *)
let get_num board s = 
  match get_box s (board) with
  | Empty -> " "
  | Guess num -> white (string_of_int num)
  | Final num -> yellow (string_of_int num)

(** [print_start_board board] is a starting board string version of [board] *)
let print_board board = "\027[1;36m    1   2   3   4   5   6   7   8   9
  \027[1;31m+-----------------------------------+
\027[1;36ma \027[1;31m|\027[0m "^get_num board "a1" ^" | "^get_num board "a2" 
                        ^" | "^get_num board "a3" ^" \027[1;31m|\027[0m "
                        ^get_num board "a4" ^" | "
                        ^get_num board "a5" ^" | "^get_num board "a6" 
                        ^" \027[1;31m|\027[0m "
                        ^get_num board "a7" ^" | "^get_num board "a8" ^" | "
                        ^get_num board "a9" 
                        ^" \027[1;31m| 
  |\027[0m---+---+---\027[1;31m+\027"
                        ^"[0m---+---+---\027[1;31m+\027[0m---+---+---\027[1;31m|
\027[1;36mb \027[1;31m|\027[0m "^get_num board "b1" ^" | "^get_num board "b2" 
                        ^" | "^get_num board "b3" ^" \027[1;31m|\027[0m "
                        ^get_num board "b4" ^" | "
                        ^get_num board "b5" ^" | "^get_num board "b6" 
                        ^" \027[1;31m|\027[0m "
                        ^get_num board "b7" ^" | "^get_num board "b8" ^" | "
                        ^get_num board "b9" 
                        ^" \027[1;31m|
  |\027[0m---+---+---\027[1;31m+\027"
                        ^"[0m---+---+---\027[1;31m+\027[0m---+---+---\027[1;31m|
\027[1;36mc \027[1;31m|\027[0m "^get_num board "c1" ^" | "^get_num board "c2" 
                        ^" | "^get_num board "c3" ^" \027[1;31m|\027[0m "
                        ^get_num board "c4" ^" | "
                        ^get_num board "c5" ^" | "^get_num board "c6" 
                        ^" \027[1;31m|\027[0m "
                        ^get_num board "c7" ^" | "^get_num board "c8" ^" | "
                        ^get_num board "c9" 
                        ^" \027[1;31m|
  \027[1;31m|-----------+-----------+-----------|
\027[1;36md \027[1;31m|\027[0m "^get_num board "d1" ^" | "^get_num board "d2" 
                        ^" | "^get_num board "d3" ^" \027[1;31m|\027[0m "
                        ^get_num board "d4" ^" | "
                        ^get_num board "d5" ^" | "^get_num board "d6" 
                        ^" \027[1;31m|\027[0m "
                        ^get_num board "d7" ^" | "^get_num board "d8" ^" | "
                        ^get_num board "d9" 
                        ^" \027[1;31m|
  |\027[0m---+---+---\027[1;31m+\027"
                        ^"[0m---+---+---\027[1;31m+\027[0m---+---+---\027[1;31m|
\027[1;36me \027[1;31m|\027[0m "^get_num board "e1" ^" | "^get_num board "e2" 
                        ^" | "^get_num board "e3" ^" \027[1;31m|\027[0m "
                        ^get_num board "e4" ^" | "
                        ^get_num board "e5" ^" | "^get_num board "e6" 
                        ^" \027[1;31m|\027[0m "
                        ^get_num board "e7" ^" | "^get_num board "e8" ^" | "
                        ^get_num board "e9" 
                        ^" \027[1;31m|
  |\027[0m---+---+---\027[1;31m+\027"
                        ^"[0m---+---+---\027[1;31m+\027[0m---+---+---\027[1;31m|
\027[1;36mf \027[1;31m|\027[0m "^get_num board "f1" ^" | "^get_num board "f2" 
                        ^" | "^get_num board "f3" ^" \027[1;31m|\027[0m "
                        ^get_num board "f4" ^" | "
                        ^get_num board "f5" ^" | "^get_num board "f6" 
                        ^" \027[1;31m|\027[0m "
                        ^get_num board "f7" ^" | "^get_num board "f8" ^" | "
                        ^get_num board "f9" 
                        ^" \027[1;31m|
  \027[1;31m|-----------+-----------+-----------|
\027[1;36mg \027[1;31m|\027[0m "^get_num board "g1" ^" | "^get_num board "g2" 
                        ^" | "^get_num board "g3" ^" \027[1;31m|\027[0m "
                        ^get_num board "g4" ^" | "
                        ^get_num board "g5" ^" | "^get_num board "g6" 
                        ^" \027[1;31m|\027[0m "^get_num board "g7" ^" | "
                        ^get_num board "g8" ^" | "^get_num board "g9" 
                        ^" \027[1;31m|
  |\027[0m---+---+---\027[1;31m+\027"
                        ^"[0m---+---+---\027[1;31m+\027[0m---+---+---\027[1;31m|
\027[1;36mh \027[1;31m|\027[0m "^get_num board "h1" ^" | "
                        ^get_num board "h2" ^" | "^get_num board "h3" 
                        ^" \027[1;31m|\027[0m "
                        ^get_num board "h4" ^" | "^get_num board "h5" ^" | "
                        ^get_num board "h6" ^" \027[1;31m|\027[0m "
                        ^get_num board "h7" ^" | "^get_num board "h8" ^" | "
                        ^get_num board "h9" ^" \027[1;31m|
  |\027[0m---+---+---\027[1;31m+\027"
                        ^"[0m---+---+---\027[1;31m+\027[0m---+---+---\027[1;31m|
\027[1;36mi \027[1;31m|\027[0m "^get_num board "i1" ^" | "^get_num board "i2" 
                        ^" | "^get_num board "i3" ^" \027[1;31m|\027[0m "
                        ^get_num board "i4" ^" | "^get_num board "i5" ^" | "
                        ^get_num board "i6" ^" \027[1;31m|\027[0m "
                        ^get_num board "i7" ^" | "^get_num board "i8" ^" | "
                        ^get_num board "i9" ^" \027[1;31m|
  \027[1;31m+-----------------------------------+\027[0m
  "

(** [print_board board] is a string version of [board] *)
let print_start_board board = "\027[1;36m    1   2   3   4   5   6   7   8   9
  \027[1;31m+-----------------------------------+
\027[1;36ma \027[1;31m|\027[0m "^get_num board "a1" ^" | "^get_num board "a2" 
                              ^" | "^get_num board "a3" ^" \027[1;31m|\027[0m "
                              ^get_num board "a4" ^" | "
                              ^get_num board "a5" ^" | "^get_num board "a6" 
                              ^" \027[1;31m|\027[0m "
                              ^get_num board "a7" ^" | "^get_num board "a8" ^
                              " | " ^get_num board "a9" 
                              ^" \027[1;31m|\027[1;36m        Sudoku Rules:
  \027[1;31m|\027[0m---+---+---\027[1;31m+\027"
                              ^"[0m---+---+---\027[1;31m+\027[0m---+---+--"^
                              "-\027[1;31m|\027[0m        "^
                              "Each row, column, and square (9 spaces each)
\027[1;36mb \027[1;31m|\027[0m "^get_num board "b1" ^" | "^get_num board "b2" 
                              ^" | "^get_num board "b3" ^" \027[1;31m|\027[0m "
                              ^get_num board "b4" ^" | "
                              ^get_num board "b5" ^" | "^get_num board "b6" 
                              ^" \027[1;31m|\027[0m "
                              ^get_num board "b7" ^" | "^get_num board "b8" 
                              ^" | " ^get_num board "b9" 
                              ^" \027[1;31m|\027[0m       "^
                              " needs to be filled out with the numbers 1 - 9
  \027[1;31m|\027[0m---+---+---\027[1;31m+\027"
                              ^"[0m---+---+---\027[1;31m+\027[0m---+---+---"^
                              "\027[1;31m|\027[0m        "^
                              "without repreating any numbers within the row,
\027[1;36mc \027[1;31m|\027[0m "^get_num board "c1" ^" | "^get_num board "c2" 
                              ^" | "^get_num board "c3" ^" \027[1;31m|\027[0m "
                              ^get_num board "c4" ^" | "
                              ^get_num board "c5" ^" | "^get_num board "c6" 
                              ^" \027[1;31m|\027[0m "
                              ^get_num board "c7" ^" | "^get_num board "c8" ^
                              " | " ^get_num board "c9" 
                              ^" \027[1;31m|\027[0m        column, or square.
  \027[1;31m|-----------+-----------+-----------|        
\027[1;36md \027[1;31m|\027[0m "^get_num board "d1" ^" | "^get_num board "d2" 
                              ^" | "^get_num board "d3" ^" \027[1;31m|\027[0m "
                              ^get_num board "d4" ^" | "
                              ^get_num board "d5" ^" | "^get_num board "d6" 
                              ^" \027[1;31m|\027[0m "
                              ^get_num board "d7" ^" | "^get_num board "d8" ^
                              " | " ^get_num board "d9" 
                              ^" \027[1;31m|\027[1;36m        Use the following "^
                              "commands to complete the puzzle:
  \027[1;31m|\027[0m---+---+---\027[1;31m+\027"
                              ^"[0m---+---+---\027[1;31m+\027[0m---+---+-"^
                              "--\027[1;31m|\027[0m        - enter (number) in"^
                              " (board position)
\027[1;36me \027[1;31m|\027[0m "^get_num board "e1" ^" | "^get_num board "e2" 
                              ^" | "^get_num board "e3" ^" \027[1;31m|\027[0m "
                              ^get_num board "e4" ^" | "
                              ^get_num board "e5" ^" | "^get_num board "e6" 
                              ^" \027[1;31m|\027[0m "
                              ^get_num board "e7" ^" | "^get_num board "e8" ^" | "
                              ^get_num board "e9" 
                              ^" \027[1;31m|\027[0m        - check
  \027[1;31m|\027[0m---+---+---\027[1;31m+\027"
                              ^"[0m---+---+---\027[1;31m+\027[0m---+---+---"^
                              "\027[1;31m|\027[0m        - clear        
\027[1;36mf \027[1;31m|\027[0m "^get_num board "f1" ^" | "^get_num board "f2" 
                              ^" | "^get_num board "f3" ^" \027[1;31m|\027[0m "
                              ^get_num board "f4" ^" | "
                              ^get_num board "f5" ^" | "^get_num board "f6" 
                              ^" \027[1;31m|\027[0m "
                              ^get_num board "f7" ^" | "^get_num board "f8" ^
                              " | " ^get_num board "f9" 
                              ^" \027[1;31m|\027[0m        - hint
  \027[1;31m|-----------+-----------+-----------|\027[0m        - help
\027[1;36mg \027[1;31m|\027[0m "^get_num board "g1" ^" | "^get_num board "g2" 
                              ^" | "^get_num board "g3" ^" \027[1;31m|\027[0m "
                              ^get_num board "g4" ^" | "
                              ^get_num board "g5" ^" | "^get_num board "g6" 
                              ^" \027[1;31m|\027[0m "^get_num board "g7" ^" | "
                              ^get_num board "g8" ^" | "^get_num board "g9" 
                              ^" \027[1;31m|\027[0m        - quit
  \027[1;31m|\027[0m---+---+---\027[1;31m+\027"
                              ^"[0m---+---+---\027[1;31m+\027[0m---+---+--"^
                              "-\027[1;31m|
\027[1;36mh \027[1;31m|\027[0m "^get_num board "h1" ^" | "
                              ^get_num board "h2" ^" | "^get_num board "h3" 
                              ^" \027[1;31m|\027[0m "
                              ^get_num board "h4" ^" | "^get_num board "h5" 
                              ^" | "^get_num board "h6" ^" \027[1;31m|\027[0m "
                              ^get_num board "h7" ^" | "^get_num board "h8" 
                              ^" | "^get_num board "h9" ^" \027[1;31m|
  \027[1;31m|\027[0m---+---+---\027[1;31m+\027"
                              ^"[0m---+---+---\027[1;31m+\027[0m---+---+-"^
                              "--\027[1;31m|
\027[1;36mi \027[1;31m|\027[0m "^get_num board "i1" ^" | "^get_num board "i2" 
                              ^" | "^get_num board "i3" ^" \027[1;31m|\027[0m "
                              ^get_num board "i4" ^" | "^get_num board "i5" ^
                              " | "^get_num board "i6" ^" \027[1;31m|\027[0m "
                              ^get_num board "i7" ^" | "^get_num board "i8" ^
                              " | "^get_num board "i9" ^" \027[1;31m|
  \027[1;31m+-----------------------------------+\027[0m
  "

(** [print_sudoku] prints out the word sudoku. *)
let print_sudoku = cyan("
                    __      __
     ___  __ __ ___/ / __  / /__ __ __
    (_-< / // // _  // _ //_ '_// // /
   /___//_,_ //_,_ //___//_//_//_,_ /
                                  ")

(** [enter_command num_lst state] reads the key and value of [num_lst] and 
    carries out the move command to enter the value in the board.
    Requires:
      [board] is a valid Adventure
      [state] is a valid state *)
let enter_command num_lst state = 
  match num_lst with
  | [] -> Illegal
  | h::t -> let key = h in 
    match t with 
    | [] -> Illegal
    | h::t -> move key (int_of_string h) state

(** [enter_gen diff state] reads the desired generative difficulty [diff] 
    and creates a state containing a random board of the desired difficulty.*)
let enter_gen diff state = 
  generate_state diff;;

(** [read () board state] reads the input sentence and matches the command with
    Enter, Check, Quit, or exceptions. If Enter, run enter_command.
    Requires:
      [board] is a valid Adventure
      [state] is a valid state *)
let rec read () board state = 
  try let input = to_command (read_line ()) in
    match input with
    | Enter num -> enter_command num state
    | Check -> check_state board state
    | Generate diff -> enter_gen diff state
    | Clear -> clear_state state
    | Hint -> hint_state board state
    | Help -> help_state state
    | Quit -> printf("Final Score: ");
      printf "%d\n" (get_score state);
      print_endline("Thank you for playing!"); exit 0
  with
  | e -> print_endline("Please enter a valid key and value");
    read () board state

(** [play_sudoku_helper board state] reads the input and matches with either 
    a Legal or Illegal command. If Legal, print the new board.
    If Illegal, print error message. Run till read quits.
    Requires:
      [board] is a valid Board
      [state] is a valid state *)
let rec play_sudoku_helper board state =
  let play = read () board state in
  ANSITerminal.resize 42 28;
  ANSITerminal.erase ANSITerminal.Above;
  print_endline(print_sudoku);
  match play with
  | Legal s -> print_endline(print_board (get_game s));
    print_endline(cyan("Please enter a command: "));
    play_sudoku_helper board s
  | Illegal -> print_endline(print_board (get_game state));
    print_endline(cyan("Please enter a valid command: "));
    play_sudoku_helper board state
  | Winner -> print_endline(print_board (get_game state));
    print_endline("Congratulations! You have completed the puzzle!");
    printf("Final Score: ");
    printf "%d\n" (get_score state);
    exit 0
  | NoHint -> print_endline(print_board (get_game state));
    print_endline(cyan("You have already used your hint or only"
                       ^" have one space left"));
    print_endline(cyan("Please enter a valid key and value: "));
    play_sudoku_helper board state
  | Help -> print_endline(print_start_board (get_game state));
    ANSITerminal.resize 98 28;
    print_endline(cyan("Please enter a command: "));
    play_sudoku_helper board state

(** [play_sudoku f] starts the adventure in file [f]. *)
let play_sudoku f =
  let file = Yojson.Basic.from_file f in 
  let board = make_game file in 
  let state = init_state board in
  ANSITerminal.resize 98 28;
  print_endline(print_sudoku);
  print_endline(print_start_board (get_game state));
  print_endline(cyan("Please enter a command: "));
  play_sudoku_helper board state

(** [play_sudoku f] starts the adventure in file [f]. *)
let play_sudoku_gen diff =
  let b = create_generated_board initial_gen diff in
  let board = make_game_from_gen b diff in 
  let state = init_state board in
  ANSITerminal.resize 98 28;
  print_endline(print_sudoku);
  print_endline(print_start_board (get_game state));
  print_endline(cyan("Please enter a command: "));
  play_sudoku_helper board state

(** [game ()] prompts for the game to play, then starts it. *)
let rec game () =
  ANSITerminal.(print_string [blue]
                  "\n\nWelcome to Sudoku.\n");
  print_endline "Please enter 1 for a json or 2 for a randomly generated board.\n";
  match read_line () with
  | "1" -> print_endline "Please enter name of json: ";
    begin match read_line () with 
      | exception End_of_file -> ()
      | file_name -> play_sudoku file_name
    end
  | "2" -> print_endline "Please enter difficulty: ";
    begin match read_line () with 
      | "hard" -> play_sudoku_gen Hard
      | "medium" -> play_sudoku_gen Medium
      | "easy" -> play_sudoku_gen Easy
      | exception End_of_file -> game ()
      | _ -> game ()
    end
  | _ -> game ()
;;

(* Execute the game engine. *)
let () = game ()