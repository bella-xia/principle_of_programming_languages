(*
  2024 PL Assignment 2
  
  Name                  : 
  List of Collaborators :

  Please make a good faith effort at listing people you discussed any 
  problems with here, as per the course academic integrity policy.  
  CAs/Prof need not be listed!

  Fill in the function definitions below replacing the 

    unimplemented ()

  with your code.  Feel free to add "rec" to any function listed to make
  it recursive. In some cases, you will find it helpful to define
  auxillary functions, feel free to.

  You must not use any mutation operations of OCaml for any of these
  questions: no arrays, for- or while-loops, references, etc., unless specified
  otherwise in the write-up.

  Note that you *can* use List.map etc library functions on this homework.
*)

(* Disables "unused variable" warning from dune while you're still solving these! *)
[@@@ocaml.warning "-27"]

(* Here is again our unimplemented "BOOM" function for questions you have not yet answered *)

let unimplemented _ = failwith "unimplemented"

(* ************** Section 1: Thinking like a type inferencer ************** *)

(*
   To better understand OCaml's parametric types, you are to write functions that
   return the indicated types.
   Note you can ignore the lists of type variables at the front of the types in
   the below, e.g. the `'a 'b 'c 'd.` in the `f1` type; those are OCaml notation
   meaning those types *have* to be polymorphic.  We have answered the first question
   for you to make it clear.
*)

let f0 : 'a. 'a -> int = fun _ -> 4 (* answered for you *)
let f1 : 'a 'b. 'a -> 'b -> 'b * 'a = fun (a : 'a) (b : 'b) -> (b, a)
let f2 : 'a 'b. 'a * 'b -> 'a = fun ((a, b) : 'a * 'b) -> a
let f3 : 'a 'b 'c. ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c = fun (ab : 'a -> 'b) (bc : 'b -> 'c) (a : 'a) -> bc (ab a)
let f4 : 'a 'b. 'a option -> ('a -> 'b) -> 'b option = fun (a : 'a option) (ab : 'a -> 'b) -> match a with
                                                                                              | None -> None
                                                                                              | Some a_val -> Some (ab a_val)
let f5 : 'a. 'a list list -> 'a list = fun (a : 'a list list) -> match a with
                                                                 | [] -> []
                                                                 | x :: xs -> x
let f6 : 'a 'b 'c. ('a, 'b) result -> ('b -> 'c) -> 'c option = fun (r : ('a, 'b) result) (bc : 'b -> 'c) -> match r with
                                                                                                             | Error b -> Some (bc b)
                                                                                                             | Ok _ -> None

type ('a, 'b) sometype = Left of 'a | Right of 'b

let f7 :
      'a 'b 'c 'd.
      ('a -> 'c) -> ('b -> 'c) -> ('a, 'b) sometype -> ('d, 'c) sometype =
  fun (ac : 'a -> 'c) (bc : 'b -> 'c) (st : ('a, 'b) sometype) -> match st with
                                                                  | Left a -> Right (ac a)
                                                                  | Right b -> Right (bc b)

let rec f8 : 'a 'b. ('a, 'b) sometype list -> 'a list * 'b list = 
  fun (ls : ('a, 'b) sometype list) -> match ls with 
                                       | [] -> ([], [])
                                       | x :: xs -> (let (a_ls, b_ls) = f8 xs in 
                                                        match x with 
                                                        | Left a -> (a :: a_ls, b_ls)
                                                        | Right b -> (a_ls, b :: b_ls))

(* ************** Section 2 : An ode to being lazy ************** *)

(*
   Lazy programming languages don't evaluate components of lists so it is possible to
   write an "infinite" list via OCaml pseudo-code such as

   let rec mklist n = n :: (mklist (n+1));;
   mklist 0;;

   Now, if you typed this into OCaml the `mklist 0` would unfortunately loop forever.

   **But**, if you "froze" the computation of the tail of the list by making it a
   function, we can achieve something in this lazy spirit.

   We will give you the type of lazy sequences to help you get going on this question:
*)

type 'a sequence = Nil | Sequence of 'a * (unit -> 'a sequence)

(*
   This is similar to the list type; the difference is the `unit ->` which keeps
   the tail of the list from running by making it a function.
   Here for example is how you would write an infinite sequence of zeroes:
*)
let rec zeroes = Sequence (0, fun () -> zeroes)

(*
  Here is a dummy sequence used in templates below. Remove this from the templates,
  and replace it with your answer.
*)
let unimplemented_int_sequence = Sequence (0, fun () -> unimplemented ())

(*
   It is still possible to express finite lists as sequences,
   for example here is [1; 2] as a sequence, with `Nil` denoting the empty sequence.
*)
let one_and_two = Sequence (1, fun () -> Sequence (2, fun () -> Nil))

(*
  Write a function to convert a sequence to a list. Of course if you try to 
  evaluate this on an infinite sequence such as `zeroes` above, it will not finish. 
  But we will assume sanity on the caller's part and ignore that issue in this question.  
*)

let rec list_of_sequence (s : 'a sequence) : 'a list = match s with
                                                   | Nil -> []
                                                   | Sequence (a, func_a) -> a :: list_of_sequence (func_a ())

(*
   # list_of_sequence one_and_two ;;
   - : int list = [1; 2]
*)

(*
  While it is nice to have these infinite sequences, it is often useful to "cut" 
  them to a fixed size. Write a function that cuts off a sequence after a fixed 
  non-negative number of values `n`, producing a new, potentially shorter sequence.
  
  (Treat the given count `n` as the maximum number of elements allowed in the 
  output sequence. So if the input is a finite sequence and its length is less
  than the specified count, the output sequence can have less than `n` values)
*)

let rec cut_sequence (n : int) (s : 'a sequence) : 'a sequence = match n with
                                                             | 0 -> Nil
                                                             | non_z -> let n_next = non_z - 1 in 
                                                                            match s with
                                                                            | Nil -> Nil
                                                                            | Sequence (a, func_a) -> Sequence(a, fun () -> cut_sequence n_next (func_a ()))

(*
   # list_of_sequence (cut_sequence 5 zeroes) ;;
   - : int list = [0; 0; 0; 0; 0]
*)

(*
  Create a sequence for the natural numbers, starting with 0.
*)



let naturals : int sequence = let rec get_next (n : int) : int sequence = Sequence(n, fun () -> get_next (n+1)) in 
                                      get_next 0


(*
   # list_of_sequence (cut_sequence 5 naturals) ;;
   - : int list = [0; 1; 2; 3; 4]
*)

(*
  Write a mapi function (analogous to List.mapi). This function is very similar to
  map, and the only different is that the mapper itself now takes the current index
  as an argument as well. You can find a more detailed documentation here:
  https://ocaml.org/api/List.html

  Note: We're using 0-based indexing in this function.
*)

let mapi_sequence (fn : int -> 'a -> 'b) (s : 'a sequence) : 'b sequence = 
let rec transform_sequence (fn : int -> 'a -> 'b) (s : 'a sequence) (idx : int) : 'b sequence = 
                           match s with
                           | Nil -> Nil
                           | Sequence(a, fun_a) -> Sequence((fn idx a), 
                                                            fun () -> transform_sequence fn (fun_a ()) (idx + 1)) in
        transform_sequence fn s 0

(*
   # list_of_sequence (mapi_sequence (fun i -> fun x -> i + x) one_and_two) ;;
   - : int list = [1; 3]
   # list_of_sequence (mapi_sequence (fun i -> fun _ -> i * i) one_and_two) ;;
   - : int list = [0; 1]
*)

(*
  Now, let's write an infinite sequence that represents triangle numbers!
  (see https://en.wikipedia.org/wiki/Triangular_number for more information on
   triangle numbers)
*)

let triangles : int sequence = 
  let calculate_sum (n : int) : int = n * (n + 1) / 2 in
      let rec triangle_helper (idx : int) : int sequence = Sequence((calculate_sum idx), fun () -> triangle_helper (idx+1)) in
             triangle_helper 0

(*
   # list_of_sequence (cut_sequence 10 triangles) ;;
   - : int list = [0; 1; 3; 6; 10; 15; 21; 28; 36; 45]
*)

(*
  Let's write an infinite sequence that represents the fibonacci sequence.
  (see https://en.wikipedia.org/wiki/Fibonacci_number for more information on
   the fibonacci sequence)
*)

let fibonacci : int sequence = 
  let rec fibonacci_helper (last : int option) (last_two: int option) : int sequence = match last with
                                                                                       | None -> Sequence(0, fun () -> fibonacci_helper (Some 0) None)
                                                                                       | Some last_num -> (match last_two with
                                                                                                                | None -> Sequence(1, fun() -> fibonacci_helper (Some last_num) (Some 1))
                                                                                                                | Some last_two_num -> let fibonacci_sum = last_num + last_two_num in 
                                                                                                                                        Sequence(fibonacci_sum, fun () -> fibonacci_helper (Some last_two_num) (Some fibonacci_sum))) in
  fibonacci_helper None None

(*
   # list_of_sequence (cut_sequence 12 fibonacci) ;;
   - : int = [0; 1; 1; 2; 3; 5; 8; 13; 21; 34; 55; 89]
*)

(*
  Let's define a function quite like fibonacci called "fub" as follows:
    When n is even, then fub(n) = fub(n/2)
    When n is odd, then fub(n) = fub(n-1) + fub(n+1)
    fub(0) = 0
    fub(1) = 1

  Here is a sample of the beginning of the sequence.

  [0; 1; 1; 2; 1; 3; 2; 3; 1; 4; 3; 5; 2; 5; 3]
                                       ^  ^  ^
                                       |  |  |-----------------------------
                                       | index 13 is the sum of these two |
                                       |----------------------------------|

  And those elements at indices 12 and 14 are exactly the same as indices 6 and 7.

                        ----------------------
                        |                    |
  [0; 1; 1; 2; 1; 3; 2; 3; 1; 4; 3; 5; 2; 5; 3]
                     |                 |
                     -------------------
  
  Create an infinite sequence that represents the fub sequence.
*)
let fub : int sequence = 
  let even_or_odd (n : int) : bool = let remainder = n - (n / 2) * 2 in
                                      match remainder with
                                      | 0 -> true
                                      | _ -> false in
  let rec fub_helper (idx : int) : int sequence = let is_even = even_or_odd idx in 
                                                  match is_even with
                                                  | true -> (match idx with 
                                                            | 0 -> Sequence(0, fun () -> fub_helper 1)
                                                            | _ -> let halved_result = fub_helper (idx / 2) in
                                                                           match halved_result with
                                                                           | Nil -> failwith "recursive error"
                                                                           | Sequence(halved_val, halved_fun) -> Sequence(halved_val, fun() -> fub_helper (idx + 1)))
                                                  | false -> match idx with
                                                             | 1 -> Sequence(1, fun () -> fub_helper 2)
                                                             | _ -> 
                                                            let last = fub_helper (idx - 1) in
                                                                let next = fub_helper (idx + 1) in 
                                                                    (match last with
                                                                    | Nil -> failwith "recursion error"
                                                                    | Sequence(last_num, fun_last) -> (match next with 
                                                                                                       | Nil -> failwith "recursion error"
                                                                                                       | Sequence(next_num, fun_next) -> Sequence(last_num + next_num, fun() -> next))) in
  fub_helper 0
                                                  

(*
  # list_of_sequence (cut_sequence 15 fub) ;;
  - : int list = [0; 1; 1; 2; 1; 3; 2; 3; 1; 4; 3; 5; 2; 5; 3]   
*)

(* ************** Section 3 : n-ary trees ************** *)

(*
   The following data type can be used to represent a tree
   with a variable number of children from 0 to n. Each node stores an element of
   type 'a. Each node also holds a list of 'a trees. When this list is
   empty, then the Node is implicitly a leaf node. Note that all nodes
   contain data in this representation of a tree. 
*)

type 'a n_tree = Node of 'a * 'a n_tree list

(* Here is a tree that you can use for simple tests of your functions. *)

let atree =
  Node
    ( 1,
      [
        Node
          ( 2,
            [
              Node (3, []);
              Node (4, []);
              Node (5, [ Node (6, []) ]);
              Node (7, []);
            ] );
        Node (8, []);
      ] )

(*
   Suppose that someone encodes a tree by writing a list of tuples.
   The first element is the data that is stored in the node, and the
   second is the number of children. The children trees are listed
   immediately after their parent nodes.

   For example, the list

     [("a",2); ("b",2); ("c",0); ("d",0); ("e",1); ("f",1); ("g",0)]

   represents:

             a
           /   \
         b     e
       /  \    |
      c    d   f
               |
               g

   The Node with "a" is the root and has two children, and so on.

   Write a function that takes a list that encodes a tree and returns the tree.
   Note that n_tree lacks a completely empty tree case in the type; use 
   invalid_arg appropriately if the input list is empty.
*)
let decode_tree (l : ('a * int) list) : 'a n_tree =  match l with
  | [] -> failwith "invalid argument"
  | x :: xs -> let (value, children) = x in
               let rec reduce_child (n : int) (l : ('a * int) list) : ('a n_tree list * ('a * int) list) = 
                      match n with
                      | 0 -> ([], l)
                      | _ -> match l with
                             | [] -> failwith "invalid argument"
                             | x :: xs -> let (value, children) = x in
                                   let (some_tree_list, rest_of_list) = reduce_child children xs in
                                        let (tree_list, rest_rest_of_list) =  reduce_child (n - 1) rest_of_list in
                                            (Node(value, some_tree_list) :: tree_list, rest_rest_of_list) in
             let (cur_child_list, rest_of_list) = reduce_child children xs in
                 Node(value, cur_child_list)
                  


let coded_tree =
  [ (1, 2); (2, 4); (3, 0); (4, 0); (5, 1); (6, 0); (7, 0); (8, 0) ]

(* E.G. decode_tree coded_tree = atree *)

(*
   Write a function to fold over all the elements in the tree in a preorder
   manner. This is similar to the List.fold_left function in that it 
   applies the function to the element value "on the way down" the recursion.

   E.G.
     tree_fold (fun acc n -> acc + n) 0 atree = 36
*)
let tree_fold_preorder (f : 'a -> 'b -> 'a) (acc : 'a) (tree : 'b n_tree) : 'a = 
  let rec tree_fold_helper (f : 'a -> 'b -> 'a) (acc : 'a) (tree_list : 'b n_tree list) : 'a =
    match tree_list with
    | [] -> acc
    | x :: xs -> let Node(x_val, x_children) = x in
                     let new_acc = f acc x_val in
                        let new_new_acc = tree_fold_helper f new_acc x_children in
                           tree_fold_helper f new_new_acc xs in
    let Node(root, children) = tree in
       let new_acc = f acc root in
          tree_fold_helper f new_acc children
 
(*
  Now write a function to fold over all elements in the tree in a postorder
  manner. Still fold left over all children and apply the function to the element
  value on the way back up from the recursion.
*)
let tree_fold_postorder (f : 'a -> 'b -> 'a) (acc : 'a) (tree : 'b n_tree) : 'a =
  let rec tree_fold_helper (f : 'a -> 'b -> 'a) (acc : 'a) (tree_list : 'b n_tree list) : 'a =
  match tree_list with
  | [] -> acc
  | x :: xs -> let Node(x_val, x_children) = x in
               let new_acc = tree_fold_helper f acc x_children in
                let new_new_acc = f new_acc x_val in
                   tree_fold_helper f new_new_acc xs in
    let Node(root, children) = tree in
      let new_acc = tree_fold_helper f acc children in
        f new_acc root

(*
  Write a function to find the node in the tree whose immediate children have the
  greatest sum. Your function will return both the node and the sum of its children.

  For example, consider `atree` defined above, which can be drawn like so:

          1
        /   \
       2     8
    / | | \
   3  4 5  7
        |
        6

  The node `2` has children who sum to 19, which is greater than the sum of any other
  node's children (`1` has sum 10, and `5` has sum 6).

  You will use polymorphic variants to "name" the values you're returning because we are
  returning two integers, and we don't want to confuse the two. In this example, your
  function will return
    ( `Node_value 2, `Child_sum 19 )
*)
let greatest_child_sum (tree : int n_tree) :
    [ `Node_value of int ] * [ `Child_sum of int ] =
    let rec child_sum_helper (tree_list : int n_tree list) : [ `Child_sum of int ] * int option * [ `Child_sum of int ] = 
         match tree_list with
         | [] -> (`Child_sum 0, None, `Child_sum 0) 
         | x :: xs -> let Node (root, children) = x in
                let (`Child_sum x_children_sum, x_g_node_value, `Child_sum x_g_children_sum) = child_sum_helper children in
                  let x_g_node_value_mod = match x_g_node_value with
                                           | None -> Some root
                                           | _ -> x_g_node_value in
                   let (`Child_sum xs_sum, xs_g_node_value, `Child_sum xs_g_children_sum) = child_sum_helper xs in
                       let (node_val, children_sum) = match xs_g_node_value with
                                                      | None -> (x_g_node_value_mod, x_g_children_sum)
                                                      | _ -> if (x_g_children_sum > xs_g_children_sum) 
                                                             then (x_g_node_value_mod, x_g_children_sum) 
                                                             else (xs_g_node_value, xs_g_children_sum) in
                           let cur_sum = root + xs_sum in
                              let (update_node_val, update_children_sum) = if (cur_sum > children_sum) then (None, `Child_sum cur_sum) else (node_val, `Child_sum children_sum) in
                                   (`Child_sum cur_sum, update_node_val, update_children_sum) in
          let Node(root, children) = tree in
              let(root_child_sum, g_node_val, g_children_sum) = child_sum_helper children in
                 match g_node_val with
                 | None -> (`Node_value root, root_child_sum)
                 | Some value -> (`Node_value value, g_children_sum)

(*************** Section 4: Mutable State and Memoization ******************)

(* Note: You will need to use mutable state in some form for questions in this section *)

(*
   Cache: Pure functions (those without side effects) always produces the same value
   when invoked with the same parameter. So instead of recomputing values each time,
   it is possible to cache the results to achieve some speedup.

   The general idea is to store the previous arguments the function was called
   on and its results. On a subsequent call if the same argument is passed,
   the function is not invoked - instead, the result in the cache is immediately
   returned.

   Note: For this question you don't have to worry about the case of using the cache
   for recursive calls. i.e. if you have a function, cached_factorial, we won't expect
   your function to look at the cache in the smaller recursive calls.

   e.g. let _ = cached_factorial 1 in
        let _ = cached_factorial 3 in
        cached_factorial 5

   doesn't invoke the cache, although technically 3 and 5 can both use previous computation
   to inform their calculations.
*)

(*
  Given any function f as an argument, create a function that returns a
  data structure consisting of f and its cache.
*)

type ('a, 'b) cached_fun  = (('a -> 'b) * ('a * 'b) list) ref
let new_cached_fun (f: 'a -> 'b) : ('a, 'b) cached_fun = ref (f, [])

(*
  Write a function that takes the above function-cache data structure,
  applies an argument to it (using the cache if possible) and returns
  the result.
*)
let apply_fun_with_cache (cached_fn : ('a, 'b) cached_fun) (x : 'a) : 'b = 
  let rec find_cache (cache: ('a * 'b) list) (x: 'a) : 'b option = match cache with
                                                                   | [] -> None
                                                                   | hd :: tl -> let (a, b) = hd in
                                                                        if (a = x) then Some b else find_cache tl x in
    let (fn, cache) = !cached_fn in
      let cache_query = find_cache cache x in
        match cache_query with
        | None -> let x_result = fn x in
                      let new_cache = (x, x_result) :: cache in
                         cached_fn := (fn, new_cache);
                             x_result
        | Some x_result -> x_result
      

(*
  The following function makes a cached version for f that looks
  identical to f; users can't see that values are being cached 
*)

let make_cached_fun (f : 'a -> 'b) : 'a -> 'b =
  let cf = new_cached_fun f in
  function x -> apply_fun_with_cache cf x

(*

# let f x = x + 1 ;;
- val f : int -> int = <fun>
# let cache_for_f = new_cached_fun f ;;
- val cache_for_f : ... 
# apply_fun_with_cache cache_for_f 1 ;;
- : int = 2
# cache_for_f ;;
- : ...
# apply_fun_with_cache cache_for_f 1 ;;
- : int = 2
# cache_for_f ;;
- : ...
# apply_fun_with_cache cache_for_f 2 ;;
- : int = 3
# cache_for_f ;;
- : ...
# apply_fun_with_cache cache_for_f 5 ;;
- : int = 6
# cache_for_f ;;
- : ...
# let cf = make_cached_fun f ;;
- val cf : int -> int = <fun>
# cf 4 ;;
- : int = 5
# cf 4 ;;
- : int = 5

*)
