(*
  2024 PL Assignment 1
  
  Name                  : 
  List of Collaborators :

  Please make a good faith effort at listing people you discussed any 
  problems with here, as per the course academic integrity policy.  
  CAs/Prof need not be listed!

  Fill in the function definitions below replacing the 

    unimplemented ()

  with your code.
  
  Here are a few things to note:
  * Feel free to add "rec" to any function listed to make it recursive.
  * In some cases, you will find it helpful to define auxilliary functions,
    and you should feel free to do so.
  * Ask on Courselore about expected behavior in ambiguous cases. We do not mean
    to hide the expected behavior from you, and we hope the given descriptions
    and examples are fully representative of our expectations.
  * You must not use any mutation operations of OCaml for any of these questions
    (which we have not taught yet in any case): no arrays, for- or while-loops,
    references, etc.
  * For this first assignment you can only use things in standard library Stdlib; 
    you **cannot** use list library functions such as `List.hd` or `List.nth` 
    (but you can code your own versions of them, of course).
*)

(* Disables "unused variable" warning from dune while you're still solving these! *)
[@@@ocaml.warning "-27"]

(*
  Here is a simple function which gets passed unit, (), as argument
  and raises an exception.  It is the initial implementation for each
  function below.
*)
let unimplemented () = failwith "unimplemented"

(* ************** Section One: List operations ************** *)

(*
  1a. Write a function that returns a list after removing the element at the given index in the list.
      Raise an exception if the provided index is negative or out of bounds. The first element is index 0.
      The empty list has no valid index, so an exception is always thrown on an empty list.

  # rm_nth_exn [1;2;3;4] 2;;
  - : int list = [1;2;4]
  # rm_nth_exn ["hello";"world"] 0;;
  - : string list = ["world"]
  # rm_nth_exn [] 0
  ... throws exception ... 
*)

let rec rm_nth_exn (ls : 'a list) (n : int) : 'a list = if n < 0 then failwith "negative index given" else 
                                                            match ls with 
                                                            | [] -> failwith "out of bound"
                                                            | x :: xs -> if n = 0 then xs else x :: (rm_nth_exn xs (n-1));;
 
(*
  1b. Write a function that takes a list of tuples and returns of list of the second item in the tuple.
  
  # proj2 [("hello", 1); ("world", 2)]
  - : int list = [1; 2]
  # proj2 [(100, "1001"); (200, "2001"); (0, "ACGT")]
  - : string list = ["1001"; "2001"; "ACGT"]
*)

let rec proj2 (ls : ('a * 'b) list) : 'b list = match ls with
                                                | [] -> []
                                                | x :: xs -> match x with
                                                             | (x1, x2) -> x2 :: (proj2 xs);;

(*
  1c. Write a function that filters and maps a list given a function [f] that returns Some or None. 
      For [x] in the list, if [f x] is [Some elt], then [elt] will be in the final list. Otherwise,
      [f x] is [None], and that element is ignored.

  # filter_map [1;2;3;4] (fun x -> if x mod 2 = 0 then Some (x * x) else None)
  - : int list = [4; 16]
  # filter_map [(1,2); (4,3); (5,6)] (fun (a, b) -> if a < b then None else Some (a + b))
  - : int list = [7]
*)

let rec filter_map (ls : 'a list) (f : 'a -> 'b option) : 'b list = match ls with
                                                                | [] -> []
                                                                | x :: xs -> if (f x) = None then (filter_map xs f) 
                                                                             else (Option.get(f x) :: (filter_map xs f));;

(*
  1d. Write a function that returns the run-length-encoding of a given list of integers.
      The run-length-encoding of a list is a new list of tuples, where the first item in the tuple is
      an element in the list, and the second item is the number of times in a row that item occurs.
      The run-length-encoding is effectively equivalent to the list.

  # rle [4;4;1;1;1;1;1;2;2;2;2;1;1;1]
  - : (int * int) list = [(4, 2); (1, 5); (2, 4); (1, 3)]
  # rle [0;0;0;1;0;1]
  - : (int * int) list [(0, 3); (1, 1); (0, 1); (1, 1)]
*)

let rec rle (ls : int list) : (int * int) list = match ls with
                                                 | [] -> []
                                                 | x :: xs -> let xxs = (rle xs) in 
                                                 match xxs with 
                                                 | [] -> [(x, 1)]
                                                 | (xs1 :: xs2) ->
                                                 (match xs1 with
                                                 | (xs11, xs12) -> if (x = xs11) then (xs11, xs12 + 1) :: xs2 
                                                                   else (x, 1) :: (xs1 :: xs2));;

(* ************** Section Two: Nonogram verifier *************** *)

(*
  A Nonogram puzzle is a logic game on a rectangular grid with a set of clues.
  The goal of the game is to fill the grid such that the clues are satisfied.
  Each slot in the grid can be filled with a 0 or a 1. There are clues for each
  row and column, and the clues tell you the runs of the 0s in that row or column.

  Here is a full description of the game:
  https://puzzlemadness.co.uk/nonograms/medium#rules

  The "purple" squares in the link above are equivalent to a 0 in our representation.

  An example of a correctly solved Nonogram puzzle is shown below:

                                     1 
             4  3  3  2  2  2  5     3
          3  1  1  2  1  3  5  4  10 1
         -----------------------------
    10  | 0  0  0  0  0  0  0  0  0  0 
     9  | 0  0  0  0  0  0  0  0  0  1
  4  2  | 0  0  0  0  1  1  1  0  0  1
  1  3  | 1  0  1  1  1  1  0  0  0  1
     5  | 1  1  1  1  1  0  0  0  0  0
  2  2  | 1  1  1  1  1  0  0  1  0  0
     5  | 1  1  1  1  1  0  0  0  0  0
     3  | 1  1  1  1  1  1  0  0  0  1
  1  3  | 1  1  1  0  1  1  1  0  0  0
  4  2  | 1  0  0  0  0  1  1  0  0  1

  We see that the clues on the left are exactly the runs of zeros in that row, but they do not
  specify where the runs occur. Similarly for the column clues at the top.

  In this particular example, in the last row, the clue "4  2" shows that there are four consecutive 
  zeros, and then later there are two consecutive zeros, and there are no other zeros in that row
  except those specified by the clue.

  In this part of the assignment, we will implement an algorithm to check that the clues are satisfied.
  The algorithm is simple:
  1. Verify that the grid is rectangular
  2. For each row and column, calculate the run-length-encoding
  3. Filter the run-length-encoding to only the lengths of the "zero runs"
  4. Check that the resulting list equals the clue

  The representation of the grid will be an OCaml list. 

  let test_grid = 
    [[0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
    ;[0; 0; 0; 0; 0; 0; 0; 0; 0; 1]
    ;[0; 0; 0; 0; 1; 1; 1; 0; 0; 1]
    ;[1; 0; 1; 1; 1; 1; 0; 0; 0; 1]
    ;[1; 1; 1; 1; 1; 0; 0; 0; 0; 0]
    ;[1; 1; 1; 1; 1; 0; 0; 1; 0; 0]
    ;[1; 1; 1; 1; 1; 0; 0; 0; 0; 0]
    ;[1; 1; 1; 1; 1; 1; 0; 0; 0; 1]
    ;[1; 1; 1; 0; 1; 1; 1; 0; 0; 0]
    ;[1; 0; 0; 0; 0; 1; 1; 0; 0; 1]]

  The column clues and row clues will be separate OCaml lists.

  let test_col_clues =
    [[3]; [4;1]; [3;1]; [3;2]; [2;1]; [2;3]; [2;5]; [5;4]; [10]; [1;3;1]]

  let test_row_clues =
    [[10]; [9]; [4;2]; [1;3]; [5]; [2;2]; [5]; [3]; [1;3]; [4;2]]

  The function itself will take in three arguments:
  1. The grid
  2. The column clues
  3. The row clues

  The result of the function will only be a boolean that tells whether the grid and clues are valid.

  Before implementing the `verify_solution` function, you will implement a few required helper functions
  along the way.
*)

(*
  2a. To perform the verification, we must check that the grid is rectangular.   
      We will not require that the grid is square. It must only be rectangular.

  # is_rectangular [[0;1]; [0;1]; [1;0]]
  - : bool = true
  # is_rectangular [[0;0;1]; [0;1;1]]
  - : bool = true
  # is_rectangular [[0]; [0;1]; [1]]
  - : bool = false
*)

let rec rectangular_helper (list1 : 'a list) (list2 : 'a list) : bool = match list1 with
                                                                        | [] -> (match list2 with 
                                                                                | [] -> true
                                                                                | _-> false)
                                                                        | x :: xs -> (match list2 with
                                                                                     | [] -> false
                                                                                     | a :: ab -> (true && rectangular_helper xs ab));;
                                                                                

let rec is_rectangular (grid : 'a list list) : bool = match grid with
                                                      | [] -> true
                                                      | x :: xs -> let xs_bool = (is_rectangular xs) in
                                                                   match xs with
                                                                   | [] -> xs_bool
                                                                   | xs1 :: xs2 -> (rectangular_helper x xs1) && xs_bool;;

(*
  2b. We'll need to check that two lists are equal. We will do this with an `equal` parameter.

  # equal_list [1;2;3] [1;2;3] Int.equal
  - : bool = true
  # equal_list ["hello"; "world"] ["world"; "hello"] String.equal
  - : bool = false
*)

let rec equal_list (a : 'a list) (b : 'a list) (equal : 'a -> 'a -> bool) : bool = match a with 
                                                                                  | [] -> (match b with
                                                                                           | [] -> true
                                                                                           | _ -> false)
                                                                                  | x :: xs -> (match b with
                                                                                               | m :: mn -> (equal m x) && (equal_list xs mn equal)
                                                                                               | _ -> false);;

(*
  2c. It's now time to verify the entire solution. You will likely find a few other functions helpful
      before writing this, but it depends on your implementation.
      
      Some suggestions are
        equal_grid : 'a list list -> 'a list list -> bool
        list_map : 'a list -> ('a -> 'b) -> 'b list

      ... and maybe
        transpose : 'a list list -> 'a list list

      Feel free to create these helper functions before beginning on verify_solution. Also, you are welcome
      to use any functions you wrote in Section One.

  # verify_solution test_grid test_col_clues test_row_clues
  - : bool = true
*)

let rec get_head_tail (ls : 'a list list) : ('a list * 'a list list) = match ls with
                                                                       | [] -> ([], [])
                                                                       | x :: xs -> 
                                                                        let (return_list, return_list_list) = get_head_tail xs in
                                                                              (match x with
                                                                              | [] -> (return_list, return_list_list) 
                                                                              | hd :: tl -> (hd :: return_list, tl :: return_list_list));;

let rec transpose (ls: 'a list list) : 'a list list = let (first_col, others) = get_head_tail ls in 
                                                           match first_col with
                                                           | [] -> []
                                                           | _ -> first_col :: (transpose others);;

let rec equal_grid (a : 'a list list) (b : 'a list list) : bool = match a with 
                                                                  | [] -> (match b with 
                                                                           | [] -> true
                                                                           | _ -> false)
                                                                  | x :: xs -> (match b with
                                                                  | m :: mn -> (equal_list x m (fun aa bb -> (aa = bb))) && (equal_grid xs mn)
                                                                  | _ -> false);;

let rec zero_counter (ls : int list) : int list = match ls with
                                                  | [] -> []
                                                  | x :: xs -> let xs_zeros = (zero_counter xs) in 
                                                               if x <> 0 then xs_zeros else 
                                                               (match xs with
                                                               | [] -> [1]
                                                               | x1 :: xs1 -> if (x1 = 0) then (match xs_zeros with
                                                                                               | n :: ns -> (n + 1) :: ns
                                                                                               | _ -> failwith "invalid") 
                                                                              else (1 :: xs_zeros));;

let verify_solution (grid : int list list) (col_clues : int list list) (row_clues : int list list) : bool =  
                                  (is_rectangular grid) &&
                                  let rec build_row (grid : int list list) : int list list = (match grid with 
                                                            | [] -> []
                                                            | x :: xs -> (zero_counter x) :: (build_row xs)) in
                                  (
                                      let row_grid = build_row grid in
                                            equal_grid row_grid row_clues
                                  ) 
                                  &&
                                  (
                                  let t_grid = transpose grid in
                                     let row_tgrid = build_row t_grid in
                                        equal_grid row_tgrid col_clues
                                  );;


(* let rec list_map (ls : 'a list) (f: 'a  -> 'b) : 'b list = match ls with
                                                          | [] -> []
                                                          | x :: xs -> (f x) :: (list_map xs f);;


let rec get_nth_grid (ls : 'a list list) ( n : int ): 'a list = let rec get_nth (ls : 'a list) ( n : int ): 'a = match ls with
                                                                       | [] -> failwith "invalid list"
                                                                       | x :: xs -> if n = 0 then x else (get_nth xs (n-1)) in
                                                                          (match ls with
                                                                             | [] -> []
                                                                             | x :: xs -> (get_nth x n) :: (get_nth_grid xs n));;
    
                                                                                            

let verify_solution_v1 (grid : int list list) (col_clues : int list list) (row_clues : int list list) : bool =  
                    (is_rectangular grid) &&
                    (let rec build_row (grid : int list list) : int list list = (match grid with 
                                              | [] -> []
                                              | x :: xs -> (zero_counter x) :: (build_row xs)) in
                        let row_grid = build_row grid in
                              equal_grid row_grid row_clues
                    ) 
                    &&
                    (let num_cols = let rec find_num_col (ls : 'a list) : int = match ls with
                    | [] -> 0
                    | x :: xs -> 1 + (find_num_col xs) in
                      match grid with
                                    | [] -> 0
                                    | x :: xs -> find_num_col(x) in
                      let rec build_col (grid : int list list) (n : int) (idx : int) : int list list = if n = idx then []
                                                                                                       else zero_counter (get_nth_grid grid n) :: (build_col grid (n+1) idx) in 
                            let col_grid = build_col grid 0 num_cols in
                                equal_grid col_grid col_clues);; *)
                                   