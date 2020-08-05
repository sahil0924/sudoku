
type answer = string list

type difficulty = 
  | Hard
  | Medium
  | Easy
  | Bad

type command = 
  | Enter of answer
  | Check
  | Generate of difficulty
  | Clear
  | Hint
  | Help
  | Quit

exception Empty

exception Incorrect

(** [enter str_lst acc] is the string list containing the command in enter.
    For example, the output may be ["a4"; "4"] *)
let rec enter str_lst acc = 
  match str_lst with 
  | [] -> acc
  | h::t -> if h <> "in" then enter t (h::acc) else enter t acc

(** [diff] matches the string representing a type difficulty. *)
let diff = function
  | "hard"::t -> Hard
  | "medium"::t -> Medium
  | "easy"::t -> Easy 
  | _ -> Bad;;

let to_command str = 
  if String.length str = 0 then raise Empty else 
    let c = String.split_on_char ' ' str in 
    match c with
    | [] -> raise Incorrect
    | h::t -> if h = "quit" then Quit
      else if h = "check" then Check
      else if h = "generate" then Generate (diff t)
      else if h = "clear" then Clear
      else if h = "hint" then Hint
      else if h = "help" then Help
      else if h = "enter" then Enter (enter t []) 
      else raise Incorrect