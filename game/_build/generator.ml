open Random
open Format
open Board
open Command

type t = {
  resDic: int array;
  validRowVals: (int * int array) list;
  validColVals: (int * int array) list;
  validBoxVals: (int * int array) list;
  tried: (int * int array) list;
  tracking: bool;
}

let rec initial_gen_helper t = function
  | 9 -> t
  | x -> initial_gen_helper {
      validRowVals = t.validRowVals @ [(x, [| 1; 2; 3; 4; 5; 6; 7; 8; 9|])];
      validColVals = t.validColVals @ [(x, [| 1; 2; 3; 4; 5; 6; 7; 8; 9|])];
      resDic = t.resDic;
      tried = t.tried;
      tracking = t.tracking;
      validBoxVals = t.validRowVals @ [(x, [| 1; 2; 3; 4; 5; 6; 7; 8; 9|])];
    } (x+1);;

let initial_gen = 
  let rec helper lst count = 
    match count with 
    | 81 -> lst
    | x -> helper ([(x, [||])]@lst) (count + 1) in 
  let tr = helper [] 0 in
  let start = {
    validRowVals = [];
    validColVals = [];
    resDic = Array.make 81 0;
    tried = tr;
    tracking = false;
    validBoxVals = [];
  } in
  initial_gen_helper start 0;;

let get_random lst = 
  let n = Random.int (Array.length lst) in
  Array.get lst n;;

let get_previous_space spaceR spaceC = 
  if spaceC = 0 then (spaceR - 1, 8)
  else (spaceR, spaceC - 1);;

let get_intersection (lst: int array) (lst2: int array) = 
  Array.fold_left (fun a x -> 
      if Array.mem x lst2 then Array.concat [[|x|]; a] else a) [||] lst;;

let get_set lst lst2 = 
  if Array.length lst2 > 0 && Array.length lst > 0 then Array.fold_left 
      (fun a x -> 
         if Array.mem x lst2 then a else Array.concat [[|x|]; a]) [||] lst 
  else lst;;

let set_dic index value arr = 
  Array.mapi (fun i x -> if i = index then value else x) arr;;

let getBox row col = 
  match row, col with 
  | x, y when x < 3 && y < 3 -> 0
  | x, y when x < 3 && y < 6 -> 1
  | x, y when x < 3 && y < 9 -> 2
  | x, y when x < 6 && y < 3 -> 3
  | x, y when x < 6 && y < 6 -> 4
  | x, y when x < 6 && y < 9 -> 5
  | x, y when x < 9 && y < 3 -> 6
  | x, y when x < 9 && y < 6  -> 7
  | x, y when x < 9 && y < 9 -> 8
  | _ -> failwith "invalid box";;

let backtrack t spaceR spaceC tracking = 
  let space = get_previous_space spaceR spaceC in 
  match space with 
  | (x, y) -> let index = (x*9) + y in
    let box = getBox x y in 
    let val_at_space = Array.get t.resDic index in 
    {
      validRowVals = List.map (fun i -> begin
            match i with 
            | (x2, y2) -> if x2 = x then 
                (x2, (Array.concat [[|val_at_space|]; y2])) else (x2, y2)
          end) t.validRowVals;
      validColVals = List.map (fun i -> begin
            match i with 
            | (x2, y2) -> if x2 = y then 
                (x2, (Array.concat [[|val_at_space|]; y2])) else (x2, y2)
          end) t.validColVals;
      resDic = set_dic index 0 t.resDic;
      tried = List.map (fun i -> begin
            match i with 
            | (x2, lst) -> if x2 = index then 
                (x2, Array.concat [lst; [|val_at_space|]])
              else (x2, lst) end) t.tried;
      tracking = true;
      validBoxVals = List.map (fun i -> begin
            match i with 
            | (x2, y2) -> if x2 = box then 
                (x2, (Array.concat [[|val_at_space|]; y2])) else (x2, y2)
          end) t.validBoxVals;
    }
;;

let rec remove_from_array arr value acc count curr : (int * int array) list = 
  match count with 
  | z when z >= Array.length arr -> [(curr, acc)] 
  | x -> let n = Array.get arr x in if n = value 
    then remove_from_array arr value acc (count+1) curr
    else remove_from_array arr value (Array.concat [acc; [|n|]]) (count+1) curr;;

let rec board_helper spaceR spaceC t = 
  match spaceR, spaceC with 
  | 9, _ -> t 
  | _, 9 -> t 
  | x, y -> let track = t.tracking in 
    let box = getBox spaceR spaceC in 
    let index = (spaceR*9) + spaceC in 
    let iNoTrack = 
      get_intersection (get_intersection (List.assoc spaceR t.validRowVals)
                          (List.assoc spaceC t.validColVals)) 
        (List.assoc box t.validBoxVals) in
    let i = (if track then (get_set iNoTrack (List.assoc index t.tried)) 
             else iNoTrack) in
    if (Array.length i > 0) then 
      let choice = get_random i in 
      let res = {
        validRowVals = (remove_from_array (List.assoc spaceR t.validRowVals) 
                          choice [||] 0 spaceR)
                       @(List.remove_assoc spaceR t.validRowVals);
        validColVals = (remove_from_array (List.assoc spaceC t.validColVals) 
                          choice [||] 0 spaceC)
                       @(List.remove_assoc spaceC t.validColVals);
        resDic = set_dic index choice t.resDic;
        tried = t.tried;
        tracking = false;
        validBoxVals = (remove_from_array (List.assoc box t.validBoxVals) 
                          choice [||] 0 box)
                       @(List.remove_assoc box t.validBoxVals);
      } in 
      if spaceC < 8 then
        board_helper spaceR (spaceC+1) res
      else 
        board_helper (spaceR + 1) 0 res
    else 
      let res = backtrack t spaceR spaceC false in 
      match get_previous_space spaceR spaceC with 
      | (p, q) -> 
        board_helper p q res;;

let rec to_board_matrix (arr: int array) count acc = 
  match count with 
  | 9 -> (to_board acc)
  | z -> let x = z*9 in 
    let lst = (Array.sub arr x 9) |> Array.to_list in 
    to_board_matrix arr (count+1) (acc@[lst]);;

let rec create_deconstructed_board arr count = 
  let rec get_true_random_box arr i =
    match arr.(i) with 
    | 0 -> get_true_random_box arr (Random.int 81)
    | x -> arr.(i) <- 0 in
  let old = Array.copy arr in 
  get_true_random_box arr (Random.int 81);
  get_true_random_box arr (Random.int 81);
  get_true_random_box arr (Random.int 81);
  let t = to_board_matrix arr 0 [] in
  match count with 
  | 0 -> to_board_matrix old 0 []
  | c -> begin
      match solution t with 
      | OneSol x -> create_deconstructed_board arr (c - 1)
      | MultSol -> create_deconstructed_board old c 
      | NoSol -> create_deconstructed_board old c
    end

let create_generated_board t diff = 
  let a = board_helper 0 0 t in 
  match diff with 
  | Hard -> create_deconstructed_board a.resDic 15
  | Medium -> create_deconstructed_board a.resDic 10
  | Easy -> create_deconstructed_board a.resDic 5
  | Bad -> failwith "bad input";;

let get_result t = 
  t.resDic;;