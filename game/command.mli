(** Going through player commands *)

(** The type [answer] represents the answer that can be part of a 
    player command. There are only two elements of the list: the key and value
    the player wants to enter in the sudoku board.  For example:
    - If the player command is ["enter 8 in a4"], then the object phrase is 
      [["a4"; "8"]].

    An [answer] is not permitted to be the empty list. *)
type answer = string list

(** The type [difficulty] represents the desired difficulty of a board *)
type difficulty = 
  | Hard
  | Medium
  | Easy
  | Bad

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an answer. *)
type command = 
  | Enter of answer
  | Check
  | Generate of difficulty
  | Clear
  | Hint
  | Help
  | Quit

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Incorrect


(** [to_command] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the object phrase.
    Examples: 
    - [to_command "    enter 8 in a4   "] is [Enter ["a4"; "8"]]
    - [to_command "quit"] is [Quit]. 
    - [to_command "check"] is [Check].

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed}.*)
val to_command : string -> command
