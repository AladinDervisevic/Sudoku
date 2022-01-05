
(*-------------------------------------------------------------------*)
(* Iz model.ml *)
(*-------------------------------------------------------------------*)

(* Pomožni tip, ki predstavlja mrežo *)

type 'a grid = 'a Array.t Array.t

(* Funkcije za prikaz mreže.
   Te definiramo najprej, da si lahko z njimi pomagamo pri iskanju napak. *)

(* Razbije seznam [lst] v seznam seznamov dolžine [size] *)
let chunkify size lst =
  let rec aux chunk chunks n lst =
    match (n, lst) with
    | _, [] when chunk = [] -> List.rev chunks
    | _, [] -> List.rev (List.rev chunk :: chunks)
    | 0, _ :: _ -> aux [] (List.rev chunk :: chunks) size lst
    | _, x :: xs -> aux (x :: chunk) chunks (n - 1) xs
  in
  aux [] [] size lst

let string_of_list string_of_element sep lst =
  lst |> List.map string_of_element |> String.concat sep

let string_of_nested_list string_of_element inner_sep outer_sep =
  string_of_list (string_of_list string_of_element inner_sep) outer_sep

let string_of_row string_of_cell row =
  let string_of_cells =
    row |> Array.to_list |> chunkify 3
    |> string_of_nested_list string_of_cell "" "│"
  in
  "┃" ^ string_of_cells ^ "┃\n"

let print_grid string_of_cell grid =
  let ln = "───" in
  let big = "━━━" in
  let divider = "┠" ^ ln ^ "┼" ^ ln ^ "┼" ^ ln ^ "┨\n" in
  let row_blocks =
    grid |> Array.to_list |> chunkify 3
    |> string_of_nested_list (string_of_row string_of_cell) "" divider
  in
  Printf.printf "┏%s┯%s┯%s┓\n" big big big;
  Printf.printf "%s" row_blocks;
  Printf.printf "┗%s┷%s┷%s┛\n" big big big

(* Funkcije za dostopanje do elementov mreže *)

let get_row (grid : 'a grid) (row_ind : int) = 
  Array.init 9 (fun col_ind -> grid.(row_ind).(col_ind))

let rows grid = List.init 9 (get_row grid)

let get_column (grid : 'a grid) (col_ind : int) =
  Array.init 9 (fun row_ind -> grid.(row_ind).(col_ind))

let columns grid = List.init 9 (get_column grid)

let get_box_ind row_ind col_ind =
  (row_ind / 3) * 3 + (col_ind / 3)

let get_box (grid : 'a grid) (box_ind : int) : 'a grid =
  let row = (box_ind / 3) * 3
  and col = (box_ind mod 3) * 3 in
  let box = Array.init 3 (
    fun i -> Array.init 3 (
      fun j -> grid.(row + i).(col + j)
      )
    )
  in
  box

let boxes grid : 'a grid list = List.init 9 (get_box grid)

(* Funkcije za ustvarjanje novih mrež *)

let map_grid (f : 'a -> 'b) (grid : 'a grid) : 'b grid = 
  Array.init 9 (fun i -> Array.map f grid.(i))

let copy_grid (grid : 'a grid) : 'a grid = map_grid (fun x -> x) grid

let foldi_grid (f : int -> int -> 'a -> 'acc -> 'acc) (grid : 'a grid)
    (acc : 'acc) : 'acc =
  let acc, _ =
    Array.fold_left
      (fun (acc, row_ind) row ->
        let acc, _ =
          Array.fold_left
            (fun (acc, col_ind) cell ->
              (f row_ind col_ind cell acc, col_ind + 1))
            (acc, 0) row
        in
        (acc, row_ind + 1))
      (acc, 0) grid
  in
  acc

let row_of_string cell_of_char str =
  List.init (String.length str) (String.get str) |> List.filter_map cell_of_char

let grid_of_string cell_of_char str =
  let grid =
    str |> String.split_on_char '\n'
    |> List.map (row_of_string cell_of_char)
    |> List.filter (function [] -> false | _ -> true)
    |> List.map Array.of_list |> Array.of_list
  in
  if Array.length grid <> 9 then failwith "Nepravilno število vrstic";
  if Array.exists (fun x -> x <> 9) (Array.map Array.length grid) then
    failwith "Nepravilno število stolpcev";
  grid

(* Model za vhodne probleme *)

type problem = { initial_grid : int option grid }

let string_of_cell (cell : int option) = match cell with
  | None -> " "
  | Some number -> string_of_int number

let print_problem problem : unit = 
  print_grid string_of_cell problem.initial_grid

let problem_of_string str =
  let cell_of_char = function
    | ' ' -> Some None
    | c when '1' <= c && c <= '9' -> Some (Some (Char.code c - Char.code '0'))
    | _ -> None
  in
  { initial_grid = grid_of_string cell_of_char str }

(* Model za izhodne rešitve *)

type solution = int grid

let print_solution solution = print_grid string_of_int solution

let rec valid_row (row : int array) : bool = 
  let len = Array.length row in
  if len <= 1 
    then true
  else
    let prvi = row.(0)
    and ostatek = Array.init (len - 1) (fun x -> row.(x + 1)) in
    if Array.exists (fun x -> x = prvi) ostatek then false 
    else valid_row ostatek

let valid_box (box : int array array) : bool =
  valid_row (Array.concat (Array.to_list box))

(* preveri če izpolnjen sudoku ustreza pravilih igre *)
(* valid_row deluje za vrstice in stolpce enako saj sta istega tipa *)
let is_valid_grid (grid : solution) : bool = 
  List.for_all valid_row (rows grid) && 
  List.for_all valid_row (columns grid) && 
  List.for_all valid_box (boxes grid)

let compare_cells (c1 : int option) (c2 : int) : bool =
  match c1 with
  | None -> true
  | Some x -> x = c2

let is_applicable_solution (problem : problem) (solution : solution) : bool = 
  let values = Array.init 9 (
    fun i -> Array.init 9 (
      fun j -> compare_cells problem.initial_grid.(i).(j) solution.(i).(j)
      )
    )
  in
  Array.for_all (fun array -> Array.for_all (fun y -> y = true) array) values

let is_valid_solution (problem : problem) (solution : solution) = 
  is_applicable_solution problem solution &&
  is_valid_grid solution

(*-------------------------------------------------------------------*)
(* Iz solver.ml *)
(*-------------------------------------------------------------------*)

type available = { loc : int * int; possible : int list }

(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)
type state = { 
  problem : problem; 
  current_grid : int option grid;
  empty_cells : available array;
  }

let print_state (state : state) : unit =
  print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of solution | Unsolved of state | Fail of state

let filter_integers (array : int option array) : int list =
  let rec aux acc list = match list with
    | [] -> acc
    | x :: xs -> match x with
      | None -> aux acc xs
      | Some digit -> aux (digit :: acc) xs
  in
  aux [] (Array.to_list array)

let init_possibilities row_ind col_ind (grid : int option grid) : int list =
  let row = filter_integers (get_row grid row_ind)
  and column = filter_integers (get_column grid col_ind) in
  let box_ind = get_box_ind row_ind col_ind in
  let unfiltered_box_as_array = 
    Array.concat (Array.to_list (get_box grid box_ind)) 
  in
  let filtered_box_as_list = filter_integers unfiltered_box_as_array in
  let rec aux acc digit =
    if digit <= 9 then
      if (List.for_all (fun x -> x != digit) row 
      && List.for_all (fun x -> x != digit) column
      && List.for_all (fun x -> x != digit) filtered_box_as_list)
        then aux (digit :: acc) (digit + 1)
      else 
        aux acc (digit + 1)
    else
      acc
  in
  aux [] 1

let initialize_empty_cells (grid : int option grid) : available array =
  let rec cells_aux acc i j : available list =
    if i <= 8 then 
      let new_j = if j = 8 then 0 else j + 1 in
      let new_i = if new_j = 0 then i + 1 else i in
      let new_acc = 
        if grid.(i).(j) = None then 
          { loc = (i,j); possible = (init_possibilities i j grid) } :: acc 
        else 
          acc 
      in
      cells_aux new_acc new_i new_j
    else 
      acc
  in
  Array.of_list (cells_aux [] 0 0)

let initialize_state (problem : problem) : state = { 
  problem = problem;
  current_grid = copy_grid problem.initial_grid;
  empty_cells = initialize_empty_cells problem.initial_grid;
}

let validate_state (state : state) : response =
  let unsolved =
    Array.exists (Array.exists Option.is_none) state.current_grid
  in
  if unsolved then Unsolved state
  else
    (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
    let solution = map_grid Option.get state.current_grid in
    if is_valid_solution state.problem solution then Solved solution
    else Fail state

let update_empty_cells cells used_cell digit = 
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> 
      let updated_cell = 
        if x.loc = used_cell.loc then
          {
            loc = x.loc; 
            possible = List.filter (fun i -> i != digit) x.possible;
          }
        else 
          x
      in
      aux (updated_cell :: acc) xs
  in
  Array.of_list (aux [] (Array.to_list cells))

let updated_grid (grid : int option grid) loc digit : int option grid = 
  let new_grid = copy_grid grid in
  let (i, j) = loc in
  new_grid.(i).(j) <- (Some digit);
  new_grid

(* Vrne celico, ki ima vsaj 2 možnosti in hkrati najmanj od vseh. *)
let cell_with_least_possibilities state = 
  if Array.length state.empty_cells = 0 then
    None
  else  
    let rec find_aux cell len cells = match cells with
      | [] -> cell
      | x :: xs -> 
        let new_len = List.length x.possible in
        if new_len < len then 
          find_aux x new_len xs
        else
          find_aux cell len xs
    in
    let arb_cell = state.empty_cells.(0) in
    let arb_len = List.length arb_cell.possible in
    let cell = 
      find_aux arb_cell arb_len (Array.to_list state.empty_cells) 
    in
    if List.length cell.possible < 2 then
      None
    else
      Some cell

let rec different_digits = 
  function
  | [] -> true
  | x::xs ->
    if List.exists (fun i -> i = x) xs then
      false
    else
      different_digits xs

let check_grid (grid : int option grid) : bool =
  let rec valid_rows = function
    | [] -> true
    | row :: xs ->
      if different_digits (filter_integers row) then
        valid_rows xs
      else
        false
  in
  let rec boxes_aux acc = function
    | [] -> acc
    | box :: xs ->
      boxes_aux ((Array.concat (Array.to_list box)) :: acc) xs
  in
  let boolean = (
    valid_rows (rows grid) && 
    valid_rows (columns grid) &&
    valid_rows (boxes_aux [] (boxes grid))
  )
  in
  boolean

let branch_state (state : state) : (state * state) option =
  (* TODO: Pripravite funkcijo, ki v trenutnem stanju poišče hipotezo, glede katere
     se je treba odločiti. Če ta obstaja, stanje razveji na dve stanji:
     v prvem predpostavi, da hipoteza velja, v drugem pa ravno obratno.
     Če bo vaš algoritem najprej poizkusil prvo možnost, vam morda pri drugi
     za začetek ni treba zapravljati preveč časa, saj ne bo nujno prišla v poštev. *)
  
  if check_grid state.current_grid then
    let empty_cell = cell_with_least_possibilities state in
    match empty_cell with
    | None -> None 
    | Some cell -> 
      let digit = List.hd cell.possible in
      let new_grid = updated_grid state.current_grid cell.loc digit in
      let s1 = {
        problem = state.problem;
        current_grid = new_grid;
        empty_cells = initialize_empty_cells new_grid;
      }
      and s2 = {
        problem = state.problem;
        current_grid = state.current_grid;
        empty_cells = update_empty_cells state.empty_cells cell digit;
      }
      in
      Some (s1, s2)
  else
    None

let clean_state state : state = 
  let rec aux acc (cells : available list) : available list = 
    match cells with
    | [] -> acc
    | x :: xs -> 
      match x.possible with
      | [] -> aux acc xs
      | digit :: [] -> 
        let (i, j) = x.loc in
        state.current_grid.(i).(j) <- (Some digit);
        aux acc xs
      | d1 :: d2 :: _ -> aux (x :: acc) xs
  in
  {
    problem = state.problem;
    current_grid = state.current_grid;
    empty_cells = Array.of_list (aux [] (Array.to_list state.empty_cells))
  }


(* pogledamo, če trenutno stanje vodi do rešitve *)
let rec solve_state (state : state) =
  (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)
  (* TODO: na tej točki je stanje smiselno počistiti in zožiti možne rešitve *)
  let cleaned_state = clean_state state in
  match validate_state cleaned_state with
  | Solved solution ->
      (* če smo našli rešitev, končamo *)
      Some solution
  | Fail fail ->
      (* prav tako končamo, če smo odkrili, da rešitev ni *)
      None
  | Unsolved state' ->
      (* če še nismo končali, raziščemo stanje, v katerem smo končali *)
      explore_state state'

and explore_state (state : state) =
  (* pri raziskovanju najprej pogledamo, ali lahko trenutno stanje razvejimo *)
  match branch_state state with
  | None ->
      (* če stanja ne moremo razvejiti, ga ne moremo raziskati *)
      None
  | Some (st1, st2) -> 
      (* če stanje lahko razvejimo na dve možnosti, poizkusimo prvo *)
      match solve_state st1 with
      | Some solution ->
          (* če prva možnost vodi do rešitve, do nje vodi tudi prvotno stanje *)
          Some solution
      | None ->
          (* če prva možnost ne vodi do rešitve, raziščemo še drugo možnost *)
          solve_state st2

let solve_problem (problem : problem) =
  problem |> initialize_state |> solve_state


(*-------------------------------------------------------------------*)
(* Iz main.ml *)
(*-------------------------------------------------------------------*)

let find_solution problem =
  let before = Sys.time () in
  let solution = solve_problem problem in
  let after = Sys.time () in
  let elapsed_time = after -. before in
  (solution, elapsed_time)

let display_solution = function
  | Some solution ->
      Printf.printf "Končna rešitev:\n";
      print_solution solution
  | None -> Printf.printf "Rešitev ne obstaja.\n"

let find_and_display_solution (problem : problem) =
  Printf.printf "Rešujem:\n";
  print_problem problem; 
  Printf.printf "\n%!";
  let response, elapsed_time = find_solution problem in
  display_solution response;
  Printf.printf "Čas reševanja: %f s.\n%!" elapsed_time

let puzzle = "
┏━━━┯━━━┯━━━┓
┃   │7  │8  ┃
┃  6│   │ 31┃
┃ 4 │  2│   ┃
┠───┼───┼───┨
┃ 24│ 7 │   ┃
┃ 1 │ 3 │ 8 ┃
┃   │ 6 │29 ┃
┠───┼───┼───┨
┃   │8  │ 7 ┃
┃86 │   │5  ┃
┃  2│  6│   ┃
┗━━━┷━━━┷━━━┛"

let () = puzzle |> problem_of_string |> find_and_display_solution