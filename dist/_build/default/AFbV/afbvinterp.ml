open Afbvast;;

(* Replace with your code here *)

(*
 * Replace this with your interpreter code.
 *)

let rec check_closed (e : expr) (ls : ident list) : bool = 
  let rec in_list (ls : 'a list) (ele : 'a) : bool = match ls with
    | [] -> false
    | x :: xs -> if (x = ele) then true else in_list xs  ele 
  in
    match e with 
    | Var id -> in_list ls id
    | Plus (a, b) -> check_closed a ls && check_closed b ls
    | Minus (a, b) -> check_closed a ls && check_closed b ls
    | Equal (a, b) -> check_closed a ls && check_closed b ls
    | And (a ,b) -> check_closed a ls && check_closed b ls
    | Or (a, b) -> check_closed a ls && check_closed b ls
    | Not a -> check_closed a ls
    | If (a, b, c) -> check_closed a ls && check_closed b ls && check_closed c ls
    | Appl (a, b) -> check_closed a ls && check_closed b ls
    | Function (a, b) -> let ls_new = a :: ls in check_closed b ls_new
    | Let (a, b, c) -> (check_closed b ls) && (let ls_new = a :: ls in check_closed c ls_new)
    | Variant (a, b) -> (check_closed b ls)
    | Match (a, b) -> (check_closed a ls) && 
                      (let rec check_match_list (l : (name * ident * expr) list) (ls : ident list) : bool = 
                        match l with
                          | [] -> true
                          | x :: xs -> 
                            (let (n, id, ex) = x in
                              let new_ls = id :: ls in
                              (check_closed ex new_ls) && (check_match_list xs ls)) in
                      check_match_list b ls)
    | _ -> true
                                                            

let rec substitution (var : ident) (body : expr) (value : expr) : expr =  
  match body with
    | Var body_id -> if (body_id = var) then value else Var body_id
    | Plus (a, b)  -> Plus ((substitution var a value), (substitution var b value))
    | Minus (a, b) -> Minus ((substitution var a value),(substitution var b value))
    | Equal (a, b) -> Equal ((substitution var a value), (substitution var b value))
    | And (a, b) -> And ((substitution var a value), (substitution var b value))
    | Or (a, b) -> Or ((substitution var a value), (substitution var b value))
    | Not a -> Not (substitution var a value)
    | If (a, b, c) -> If ((substitution var a value), (substitution var b value), (substitution var c value))
    | Function (a, b) -> if (a = var) then Function (a, b) else Function (a, (substitution var b value))
    | Appl (a, b) -> Appl ((substitution var a value), (substitution var b value))
    | Let (a, b, c) -> if (a = var) then Let (a, (substitution var b value), c) else Let (a, (substitution var b value), (substitution var c value)) 
    | Variant (a, b) -> Variant (a, (substitution var b value))
    | Match (a, b) -> let rec substitute_match_list (ls : (name * ident * expr) list) (var : ident) (value : expr) : (name * ident * expr) list = 
                          match ls with
                           | [] -> []
                           | x :: xs -> (let (n, id, ex) = x in
                                           if id = var then (n, id, ex) :: (substitute_match_list xs var value)
                                           else (n, id, (substitution var ex value)) :: (substitute_match_list xs var value)) in
                         Match ((substitution var a value), (substitute_match_list b var value))
    | terminal_expr -> terminal_expr





let rec eval e = 
  let close_check = check_closed e [] in 
    if close_check then 
    (
    match e with
    | Var id -> Var id 

    | Int int_val -> Int int_val
  
    | Bool bool_val -> Bool bool_val
  
    | Plus (a, b)  -> 
     let a_eval = eval a in 
       let b_eval = eval b in
     (
     match a_eval with
       | Int a_val -> (match b_eval with
         | Int b_val -> Int (a_val + b_val)
         | _ -> failwith "invalid integer for Add operation"
       )
       | _ -> failwith "invalid integer for Add operation"
     )
  
    | Minus (a, b) -> 
   let a_eval = eval a in 
   let b_eval = eval b in
  (
  match a_eval with
   | Int a_val -> (match b_eval with
     | Int b_val -> Int (a_val - b_val)
     | _ -> failwith "invalid integer for Minus operation"
   )
   | _ -> failwith "invalid integer for Minus operation"
  )
  
    | Equal (a, b) -> 
    let a_eval = eval a in 
    let b_eval = eval b in
  (
  match a_eval with
   | Int a_val -> (match b_eval with
     | Int b_val -> Bool (a_val = b_val)
     | _ -> failwith "invalid integer for Equal operation"
   )
   | _ -> failwith "invalid integer for Equal operation"
  )
  
    | And (a, b) -> 
    let a_eval = eval a in 
    let b_eval = eval b in
  (
  match a_eval with
   | Bool a_val -> (match b_eval with
     | Bool b_val -> Bool (a_val && b_val)
     | _ -> failwith "invalid boolean for And operation"
   )
   | _ -> failwith "invalid boolean for And operation"
  )
  
    | Or (a, b) -> 
    let a_eval = eval a in 
    let b_eval = eval b in
  (
  match a_eval with
   | Bool a_val -> (match b_eval with
     | Bool b_val -> Bool (a_val || b_val)
     | _ -> failwith "invalid boolean for Or operation"
   )
   | _ -> failwith "invalid boolean for Or operation"
  )
  
    | Not a -> 
    let a_eval = eval a in 
   (
     match a_eval with
       | Bool a_val -> Bool (not a_val)
       | _ -> failwith "invalid boolean for Not operation"
     )
                  
    | Function (a, b) -> Function (a, b)
  
    | If (a, b, c) -> 
    let a_eval = eval a in 
    (
     match a_eval with
       | Bool true -> eval b
       | Bool false -> eval c
       | _ -> failwith "invalid boolean for If operation"
    )
  
    | Appl (a, b) -> 
    let b_eval = eval b in
    let a_eval = eval a in 
    (
     match a_eval with 
       | Function (var, body) -> 
         let sub_body = substitution var body b_eval in
           eval sub_body
       | _ -> failwith "invalid use of application rule"
    )

    | Let (a, b, c) -> eval (substitution a c (eval b))

    | Variant (a, b) -> 
      let b_eval = eval b in
        Variant (a, b_eval)

    | Match (a, b) -> 
      let a_eval = eval a in
        let (n, v) = (match a_eval with 
           | Variant (_n, _v) -> (_n, _v)
           | _ -> failwith "Invalid expression for match rule") in
          let rec find_match (n : name) (v : expr) (ls : (name * ident * expr) list) : expr =
             match ls with
              | [] -> failwith "invalid match"
              | x :: xs -> (let (_n, _id, _ex) = x in 
                              if _n = n then (eval (Appl (Function (_id, _ex), v)))
                              else find_match n v xs) in
            (find_match n v b)

    | _ -> failwith "Unimplemented expressions"
    )
  
    else failwith "Unbounded expression"


