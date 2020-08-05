open OUnit2
open Board
open State
open Command

(* Tesing Plan: Our testing plan was split between OUnit and manual. We used
   glass box testing because we used our implementation code to test our 
   functions properly. We tested everything that was easily capable of testing 
   through OUnit. For example, we tested getting the board from json, entering 
   numbers, getting the values entered, keeping track of state, and all other 
   commands. We also tested all of the exceptions being raise. All specific 
   functions are listed in this file. We used multiple boards and tested using
   all areas of the board. 

   We tested the solver, generator, and our main files through manual testing. 
   We made sure our game path followed proper commands and results. We also 
   made sure each game was solvable and a correct game to solve. 

   Our testing approach proves correctness of the system because we tested
   the functionality of the board using our implemention with OUnit. This proved
   our board was capable of being played, our commands worked, and the state of
   the board changed upon command. The rest of the functionality occurs when a
   user plays a game. We tested every command, board, and action manually while
   making sure the correct prompt is return. We tested resizing and appearance
   of the board manually to look appealing too. *)

(** [make_get_box_test test key b expected_output] constructs an OUnit
    test named [test] that asserts the quality of [expected_output]
    with [union]. *)
let make_get_box_test 
    (test: string)
    (key: string)
    (b: board)
    (expected_output : box) : test = 
  test >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (get_box key b))

(** [make_winner_test test b a expected_output] constructs an OUnit
    test named [test] that asserts the quality of [expected_output]
    with [union]. *)
let make_winner_test 
    (test: string)
    (b: board)
    (a: board)
    (expected_output : bool) : test = 
  test >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (winner b a))

(** [make_clear_test test p expected_output] constructs an OUnit
    test named [test] that asserts the quality of [expected_output]
    with [union]. *)
let make_clear_test 
    (test: string)
    (p: board)
    (expected_output : board) : test = 
  test >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (clear p))

(** [make_hint_test test p a expected_output] constructs an OUnit
    test named [test] that asserts the quality of [expected_output]
    with [union]. *)
let make_hint_test 
    (test: string)
    (p: board)
    (a: board)
    (expected_output : board) : test = 
  test >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (hint p a))

(* -------------------------------------------------------------------------- *)
(** COMMAND TESTS *)
(** [make_to_command_test test str expected_output] constructs an OUnit
    test named [test] that asserts the quality of [expected_output]
    with [union]. *)
let make_to_command_test 
    (test: string)
    (str: string)
    (expected_output : command) : test = 
  test >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (to_command str))

(* -------------------------------------------------------------------------- *)
(** STATE TESTS *)
(** [make_get_game_test test st expected_output] constructs an OUnit
    test named [test] that asserts the quality of [expected_output]
    with [union]. *)
let make_get_game_test 
    (test: string)
    (st: State.t)
    (expected_output : board) : test = 
  test >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (get_game st))

(** [make_move_test test key v st expected_output] constructs an OUnit
    test named [test] that asserts the quality of [expected_output]
    with [union]. *)
let make_move_test 
    (test: string)
    (key: string)
    (v: int)
    (st: State.t)
    (expected_output : result) : test = 
  test >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (move key v st))

(** [make_help_state_test test st expected_output] constructs an OUnit
    test named [test] that asserts the quality of [expected_output]
    with [union]. *)
let make_help_state_test 
    (test: string)
    (st: State.t)
    (expected_output : result) : test = 
  test >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (help_state st))


let j_1 = Yojson.Basic.from_file "b2.json"
let g_1 = make_game j_1
let b_new = List.sort_uniq Stdlib.compare(get_puzzle g_1)
let b_1 = List.sort_uniq Stdlib.compare(set_box "i9" 3 b_new)
let b_2 = List.sort_uniq Stdlib.compare(set_box "h5" 9 b_1)
let b_3 = List.sort_uniq Stdlib.compare(set_box "e5" 9 b_1)
let state_1 = init_state g_1
let ans = get_answer g_1
let s_1 = init_state g_1
let s_2 = match move "i9" 3 s_1 with 
  | Legal (t) -> t
  | _ -> failwith"nono"
let s_3 = match move "h5" 9 s_2 with 
  | Legal (t) -> t
  | _ -> failwith"nono"
let s_4 = match move "e5" 9 s_3 with 
  | Legal (t) -> t
  | _ -> failwith"nono"

let j_2 = Yojson.Basic.from_file "b4.json"
let game_2 = make_game j_2
let state_2 = init_state game_2
let b4_blank = List.sort_uniq Stdlib.compare(get_puzzle game_2)
let b4_one = List.sort_uniq Stdlib.compare(set_box "a2" 3 (get_puzzle game_2))
let b4_two = List.sort_uniq Stdlib.compare(set_box "a3" 9 b4_one)
let b4_three = List.sort_uniq Stdlib.compare(set_box "b3" 9 b4_two)
let b_hint = List.sort_uniq Stdlib.compare
    (check (set_box "i9" 5 b_new) (get_answer g_1))

let board_tests = [
  make_get_box_test "get box a4 in board1" "a4" (get_puzzle g_1) (Final 7);
  make_get_box_test "get box f8 in board1" "f8" (get_puzzle g_1) (Final 1);
  make_get_box_test "get box i9 in board1" "i8" (get_puzzle g_1) (Final 6);
  make_get_box_test "get box h1 in board1" "h1" (get_puzzle g_1) (Final 7);
  make_get_box_test "get box a1 in board1" "a1" (get_puzzle g_1) (Final 8);
  make_get_box_test "get box b8 in board1" "b8" (get_puzzle g_1) (Final 4);
  make_get_box_test "get box g4 in board1" "g4" (get_puzzle g_1) (Final 8);
  make_get_box_test "get box a1 in board4" "a1" (get_puzzle game_2) (Final 1);
  make_get_box_test "get box b1 in board4" "b1" (get_puzzle game_2) (Final 7);
  make_get_box_test "get box b2 in board4" "b2" (get_puzzle game_2) (Final 3);
  make_get_box_test "get box i9 in board1" "i9" (get_puzzle g_1) Empty;
  make_get_box_test "get box h5 in board1" "h5" (get_puzzle g_1) Empty;
  make_get_box_test "get box e5 in board1" "e5" (get_puzzle g_1) Empty;
  make_get_box_test "get box a2 in board1" "a2" (get_puzzle game_2) Empty;
  make_get_box_test "get box a3 in board1" "a3" (get_puzzle game_2) Empty;
  make_get_box_test "get box b3 in board1" "b3" (get_puzzle game_2) Empty;
  make_get_box_test "get box after guess in i9" "i9" b_2 (Guess 3);
  make_get_box_test "get box after guess in e5" "e5" b_3 (Guess 9);
  "valid test5" >:: (fun _ ->  assert_equal (valid 4 "e5" b_new) (true));
  "numify test5" >:: (fun _ ->  assert_equal (numify "e5") (5,5));
  make_winner_test "test true" ans ans true;
  make_winner_test "test false" b_3 ans false;
  make_clear_test "clear b2 test one" b_3 b_new;
  make_clear_test "clear b2 test two" b_1 b_new;
  make_clear_test "clear b2 test three" b_2 b_new;
  make_clear_test "clear b2 test four" b_new b_new;
  make_clear_test "clear b4 test five" b4_one b4_blank;
  make_clear_test "clear b4 test six" b4_three b4_blank;
  make_hint_test "test board 2" b_new (get_answer g_1) b_hint;
]

let game_s2 = List.sort_uniq Stdlib.compare(get_game s_2)
let game_s3 = List.sort_uniq Stdlib.compare(get_game s_3)

let state_tests= [
  "state_test1" >:: (fun _ ->  assert_equal game_s2 b_1);
  "state_test2" >:: (fun _ ->  assert_equal (get_moves s_2) 1);
  "state_test3" >:: (fun _ ->  assert_equal game_s3 b_2);
  "state_test4" >:: (fun _ ->  assert_equal (get_moves s_3) 2);
  "state_test4" >:: (fun _ ->  assert_equal (get_moves s_4) 3);
  "state_test5" >:: (fun _ ->  assert_equal (move "a1" 5 s_1) (Illegal));
  "state_test2" 
  >:: (fun _ ->  assert_raises (InvalidKey)(fun () -> move "a0" 5 s_1) );
  "state_test2" 
  >:: (fun _ ->  assert_raises (InvalidKey)(fun () -> move "a0" 5 s_1) );
  "state_test2" 
  >:: (fun _ ->  assert_raises (InvalidKey)(fun () -> move "a20" 5 s_1) );
  make_get_game_test "test inital b1 board" state_1 (get_puzzle g_1);
  make_get_game_test "test board b2" state_2 (get_puzzle game_2);
  make_move_test "test move a1 in board b1" "a1" 5 state_1 Illegal;
  "no key" 
  >:: (fun _ -> assert_raises (InvalidKey)(fun () -> move "a0" 5 state_1));
  "wrong value" 
  >:: (fun _ -> assert_raises (InvalidValue) (fun () -> move "i9" 10 state_1));
  "wrong value" 
  >:: (fun _ -> assert_raises (InvalidValue) (fun () -> move "i9" 0 state_1));
  "wrong value" 
  >:: (fun _ -> assert_raises (InvalidValue) (fun () -> move "i9" 100 state_1));
  "wrong value" 
  >::(fun _ -> assert_raises (InvalidValue) (fun () -> move "i9" (-4) state_1));
  make_help_state_test "test state 1" s_1 Help;
  make_help_state_test "test state 1" s_2 Help;
  make_help_state_test "test state 1" s_3 Help;
  make_help_state_test "test state 2" state_2 Help;
]

let command_tests = [
  make_to_command_test "test quit command" "quit" Quit;
  make_to_command_test "test check command" "check" Check;
  make_to_command_test "test clear command" "clear" Clear;
  make_to_command_test "test help command" "help" Help;
  make_to_command_test "test enter command" "enter 8 in a4" (Enter ["a4"; "8"]);
  make_to_command_test "test enter command" "enter 8 8 8" 
    (Enter ["8"; "8"; "8"]);
  "inc" >:: (fun _ -> assert_raises (Incorrect)(fun () -> to_command "c"));
  "command empty" >:: (fun _ -> assert_raises (Empty)(fun () -> to_command ""));
  "incorrect" 
  >:: (fun _ -> assert_raises (Incorrect)(fun () -> to_command "chfih"));
  "incorrect" 
  >:: (fun _ -> assert_raises (Incorrect)(fun () -> to_command "ent er"));
]

let tests =
  "test suite for A1"  >::: List.flatten [
    board_tests;
    state_tests;
    command_tests;
  ]

let _ = run_test_tt_main tests