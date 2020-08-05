open Yojson.Basic.Util
open Command

type box = 
  | Guess of int
  | Final of int
  | Empty

type board = (string * box) list

(** The abstract type representing a sudoku game *)
type t = {
  puzzle: board;
  answer: board;
  description: string;
  difficulty: int
}

exception InvalidKey

exception InvalidValue

(** [to_box num] initializes num in box form *)
let to_box (num: int) : box = 
  if num = 0 then Empty else Final num

(** [form_row json] is a list of integers from the given [json] file *)
let form_row json : int list = 
  json |> member "numbers" |> to_list |> List.map to_int

let to_board (lst : (int list) list) = 
  let rec helper l acc2 (row:char) (index:int) = 
    let key = (Char.escaped row) ^ (string_of_int index) in
    match l with 
    |[] -> acc2 
    |h::t -> helper t ((key, to_box h)::acc2) row (index+1) in 
  let rec parse_rows l acc2 (row:char) = 
    let code = Char.code row in
    match l with 
    |[] -> acc2 
    |h::t -> let a = helper h [] row 1 in 
      parse_rows t (a@acc2) (Char.chr (code+1)) in 
  parse_rows lst [] 'a'

let get_puzzle t  = t.puzzle

let get_answer t = t.answer

let get_box (key: string) (g : board) : box = 
  try List.assoc key g
  with Not_found -> raise InvalidKey

let set_box (k: string) (v:int) (d : board) = 
  if v > 9 || v < 1 then raise InvalidValue else
    try (k, Guess v):: List.remove_assoc k d
    with Not_found -> raise InvalidKey

let rec check (puzzle : board) (answer : board) = 
  match puzzle with
  | [] -> puzzle
  | (key, pint)::t -> match pint, (get_box key answer) with
    | Guess p, Final int -> if p = int 
      then (key, Final int) :: check (List.remove_assoc key puzzle) answer
      else (key, Guess p) :: check t answer
    | Final p, Final int -> 
      (key, Final int) :: check (List.remove_assoc key puzzle) answer
    | Empty, Final int -> 
      (key, Empty) :: check (List.remove_assoc key puzzle) answer
    | _ -> puzzle

(** [clear_helper box] clear helper removes any guesses from the board. *)
let clear_helper box = 
  match box with
  | Final n -> Final n
  | Guess _ -> Empty
  | Empty -> Empty

let rec clear puzzle =
  match puzzle with
  | (s, box)::t -> (s, clear_helper box)::(clear t)
  | _ -> puzzle

(** [hint_list puzzle acc] makes a list of the empty keys from the board. *)
let rec hint_list puzzle acc = 
  match puzzle with 
  | [] -> acc
  | (s, box)::t -> if box = Empty then hint_list t (s::acc) 
    else hint_list t acc

(** [get_hint hints] returns the first element of the hint list. *)
let get_hint hints =
  match hints with
  | [] -> ""
  | h::t -> h

let hint puzzle answer = 
  let hints = hint_list puzzle [] in 
  if (List.length hints) < 2 then puzzle else
    let key = get_hint hints in 
    let new_puzzle = List.remove_assoc key puzzle in 
    let answer = List.assoc key answer in 
    List.sort_uniq Stdlib.compare((key, answer)::new_puzzle)

let winner puzzle answer = 
  if (List.sort_uniq Stdlib.compare puzzle) =
     (List.sort_uniq Stdlib.compare answer) then true else false





(**  The type representing a partially solved sudoku board *)
type solving = 
  | Working of board
  | Failed

type result = 
  | MultSol
  | OneSol of board
  | NoSol

let box1 = ["a1"; "a2"; "a3"; "b1"; "b2"; "b3"; "c1"; "c2"; "c3";]
let box2 = ["a4"; "a5"; "a6"; "b4"; "b5"; "b6"; "c4"; "c5"; "c6";]
let box3 = ["a7"; "a8"; "a9"; "b7"; "b8"; "b9"; "c7"; "c8"; "c9";]
let box4 = ["d1"; "d2"; "d3"; "e1"; "e2"; "e3"; "f1"; "f2"; "f3";]
let box5 = ["d4"; "d5"; "d6"; "e4"; "e5"; "e6"; "f4"; "f5"; "f6";]
let box6 = ["d7"; "d8"; "d9"; "e7"; "e8"; "e9"; "f7"; "f8"; "f9";]
let box7 = ["g1"; "g2"; "g3"; "h1"; "h2"; "h3"; "i1"; "i2"; "i3";]
let box8 = ["g4"; "g5"; "g6"; "h4"; "h5"; "h6"; "i4"; "i5"; "i6";]
let box9 = ["g7"; "g8"; "h9"; "h7"; "h8"; "h9"; "i7"; "i8"; "i9";]

let row1 = ["a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7"; "a8"; "a9";]
let row2 = ["b1"; "b2"; "b3"; "b4"; "b5"; "b6"; "b7"; "b8"; "b9";]
let row3 = ["c1"; "c2"; "c3"; "c4"; "c5"; "c6"; "c7"; "c8"; "c9";]
let row4 = ["d1"; "d2"; "d3"; "d4"; "d5"; "d6"; "d7"; "d8"; "d9";]
let row5 = ["e1"; "e2"; "e3"; "e4"; "e5"; "e6"; "e7"; "e8"; "e9";]
let row6 = ["f1"; "f2"; "f3"; "f4"; "f5"; "f6"; "f7"; "f8"; "f9";]
let row7 = ["g1"; "g2"; "g3"; "g4"; "g5"; "g6"; "g7"; "g8"; "g9";]
let row8 = ["h1"; "h2"; "h3"; "h4"; "h5"; "h6"; "h7"; "h8"; "h9";]
let row9 = ["i1"; "i2"; "i3"; "i4"; "i5"; "i6"; "i7"; "i8"; "i9";]

let col1 = ["a1"; "b1"; "c1"; "d1"; "e1"; "f1"; "g1"; "h1"; "i1";]
let col2 = ["a2"; "b2"; "c2"; "d2"; "e2"; "f2"; "g2"; "h2"; "i2";]
let col3 = ["a3"; "b3"; "c3"; "d3"; "e3"; "f3"; "g3"; "h3"; "i3";]
let col4 = ["a4"; "b4"; "c4"; "d4"; "e4"; "f4"; "g4"; "h4"; "i4";]
let col5 = ["a5"; "b5"; "c5"; "d5"; "e5"; "f5"; "g5"; "h5"; "i5";]
let col6 = ["a6"; "b6"; "c6"; "d6"; "e6"; "f6"; "g6"; "h6"; "i6";]
let col7 = ["a7"; "b7"; "c7"; "d7"; "e7"; "f7"; "g7"; "h7"; "i7";]
let col8 = ["a8"; "b8"; "c8"; "d8"; "e8"; "f8"; "g8"; "h8"; "i8";]
let col9 = ["a9"; "b9"; "c9"; "d9"; "e9"; "f9"; "g9"; "h9"; "i9";]

let group = [(1, box1); (2, box2); (3, box3); (4, box4); (5, box5); 
             (6, box6); (7, box7); (8, box8); (9, box9)]
let rows = [(1, row1); (2, row2); (3, row3); (4, row4); (5, row5); 
            (6, row6); (7, row7); (8, row8); (9, row9)]
let cols = [(1, col1); (2, col2); (3, col3); (4, col4); (5, col5); 
            (6, col6); (7, col7); (8, col8); (9, col9)]
let forward = [1; 2; 3; 4; 5; 6; 7; 8; 9]
let backward = [9; 8; 7; 6; 5; 4; 3; 2; 1]


(** The type representing sections of the board *)
type spot = 
  | Group
  | Rows
  | Cols


let numify (k : string) : int*int =  
  let f = String.get k 0 in
  let s = String.get k 1 in
  (int_of_char f - 96, int_of_char s - 48) 

(** [get_spot k s] is the either the column, group, or row, that [k] is in,
    based on what region [s] is *)
let get_spot (k : string) (s : spot) : string list= 
  let (a, b) = numify k in
  match s with
  | Group -> List.assoc ((1 + ( 3*((a-1) / 3))) + ((b -1) / 3)) group
  | Rows -> List.assoc a rows
  | Cols -> List.assoc b cols

let valid (i : int) (k : string) (bd : board): bool = 
  let row = get_spot k Rows in 
  let col = get_spot k Cols in
  let group = get_spot k Group in
  let rec valid_helper (i: int) (lst : string list) (b: board): bool =
    match lst with
    | h::t when get_box h b = Final i || get_box h b = Guess i-> false
    | _::t -> valid_helper i t b
    | _ -> true in
  valid_helper i row bd && valid_helper i col bd && valid_helper i group bd

(** [solve b] is [Working sol] where sol is the solved board of [b], or [Failed]
    is there is no solution *)
let rec solve (l : int list) (b : board) : solving = 
  let rec filler lst k b : solving = 
    match lst with
    | h::t when valid h k b -> 
      (match (k, Final h):: List.remove_assoc k b |> solve l with 
       | Failed -> filler t k b
       | Working sol -> Working sol)
    | _::t -> filler t k b
    | _ -> Failed in 
  let rec solver lst = 
    match lst with
    | (k, Empty)::t -> filler l k b
    | _::t -> solver  t
    | _ -> Working b in 
  solver b

let solution (b : board) = match solve forward b, solve backward b with
  | Failed, Failed -> NoSol
  | Working b1, Working b2 -> if b1 = b2 then OneSol b1 else MultSol
  | _ -> failwith "No no zone"

(** [get_solution b] is the solution of the board [b]
    Required: [b] only has one solution *)
let get_solution (b: board) = match solution b with
  | OneSol b -> b
  | _ -> failwith "NO NO"


let make_game json : t = 
  let p = json |> member "board" |> to_list |> List.map form_row |> to_board in 
  {
    puzzle = p;
    answer = get_solution p;
    description = json |> member "description" |> to_string;
    difficulty = json |> member "difficulty" |> to_int;
  }

let make_game_from_gen b diff = 
  let d = (match diff with
      | Hard -> 3
      | Medium -> 2
      | Easy -> 1 
      | Bad -> failwith "bad input") in
  {
    puzzle = b;
    answer = get_solution b;
    description = "Randomly generated board";
    difficulty = d;
  }

