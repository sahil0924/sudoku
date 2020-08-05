type t = {
  resDic: int array;
  validRowVals: (int * int array) list;
  validColVals: (int * int array) list;
  validBoxVals: (int * int array) list;
  tried: (int * int array) list;
  tracking: bool;

}

(**[initial_gen t] creates the initial generator*)
val initial_gen : t

(**[get_random lst] returns a random integer from the input array*)
val get_random : int array -> int

(**[get_previous_space spaceR spaceC] takes in the row index and column index,
   respectively, and returns a tuple of the previous space.  The function tries
   to subtract from the column first, and if it violates the 9x9 parameter, 
   it subtracts from the row instead.*)
val get_previous_space : int -> int -> int * int

(**[get_intersection lst lst2] computes the intersection of the two lists*)
val get_intersection : int array -> int array -> int array

(**[get_set lst lst2] returns a list of the unique elements between the two
   lists*)
val get_set : int array -> int array -> int array

(**[set_dic index value arr] sets arr.(index) to equal value and returns the array*)
val set_dic : int -> int -> int array -> int array

(**[remove_from_array arr value acc count curr] takes in an array, value, 
   accumulator, counter and current row/col.  The parses each element of the array
   until it finds a val matching value.  It then removes the value and returns 
   a tuple of the current row/col and the entire array without the value*)
val remove_from_array : int array -> int -> int array -> int -> int -> (int * int array) list 

(**[create_board t diff] takes in a generator object and a difficulty and 
   uses the helper function to return a new generator object.  The generated board sits in t.resDic and is an
   array of integers matching all the constraints of a sudoku puzzle. It is a 9x9
   puzzle*)
val create_generated_board : t -> Command.difficulty -> Board.board

(**[get_result t] returns t.resDic, the generated sudoku puzzle*)
val get_result : t -> int array

(**[create_deconstructed_board arr count] takes in an array that reps [arr] a 
   valid sudoku board and deconstructs to make it a solveable board.  [count] is 
   the number representing difficulty, and determines the number of free spaces. 
*)
val create_deconstructed_board : int array -> int -> Board.board