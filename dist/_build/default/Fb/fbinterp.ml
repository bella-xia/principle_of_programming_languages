open Fbast

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
    | LetRec (a, b, c, d) -> let ls_new = a :: ls in (check_closed d ls_new && 
                                   (let ls_new_new = b :: ls_new in check_closed c ls_new_new))
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
    | LetRec (a, b, c, d) -> if (b = var) then LetRec (a, b, c, (substitution var d value)) else LetRec (a, b, (substitution var c value), (substitution var d value))
    | terminal_expr -> terminal_expr

let rec find_recfun (ls : (ident * (ident * expr * expr)) list) (query: ident) : (ident * expr * expr) option = 
  match ls with
    | [] -> None
    | x :: xs -> let (id, express) = x in 
                   if (id = query) then Some express else find_recfun xs query

let rec eval_helper (e : expr) (ls : (ident * (ident * expr * expr)) list) : expr = 
  match e with
| Var id -> 
  let if_rec = find_recfun ls id in
    (match if_rec with
      | None -> Var id
      | Some (var, body, expression) -> Function (var, substitution id body (LetRec (id, var, body, expression))))
| Int int_val -> Int int_val

| Bool bool_val -> Bool bool_val

| Plus (a, b)  -> 
   let a_eval = eval_helper a ls in 
     let b_eval = eval_helper b ls in
   (
   match a_eval with
     | Int a_val -> (match b_eval with
       | Int b_val -> Int (a_val + b_val)
       | _ -> failwith "invalid integer for Add operation"
     )
     | _ -> failwith "invalid integer for Add operation"
   )

| Minus (a, b) -> 
 let a_eval = eval_helper a ls in 
 let b_eval = eval_helper b ls in
(
match a_eval with
 | Int a_val -> (match b_eval with
   | Int b_val -> Int (a_val - b_val)
   | _ -> failwith "invalid integer for Minus operation"
 )
 | _ -> failwith "invalid integer for Minus operation"
)

| Equal (a, b) -> 
  let a_eval = eval_helper a ls in 
  let b_eval = eval_helper b ls in
(
match a_eval with
 | Int a_val -> (match b_eval with
   | Int b_val -> Bool (a_val = b_val)
   | _ -> failwith "invalid integer for Equal operation"
 )
 | _ -> failwith "invalid integer for Equal operation"
)

| And (a, b) -> 
  let a_eval = eval_helper a ls in 
  let b_eval = eval_helper b ls in
(
match a_eval with
 | Bool a_val -> (match b_eval with
   | Bool b_val -> Bool (a_val && b_val)
   | _ -> failwith "invalid boolean for And operation"
 )
 | _ -> failwith "invalid boolean for And operation"
)

| Or (a, b) -> 
  let a_eval = eval_helper a ls in 
  let b_eval = eval_helper b ls in
(
match a_eval with
 | Bool a_val -> (match b_eval with
   | Bool b_val -> Bool (a_val || b_val)
   | _ -> failwith "invalid boolean for Or operation"
 )
 | _ -> failwith "invalid boolean for Or operation"
)

| Not a -> 
  let a_eval = eval_helper a ls in 
 (
   match a_eval with
     | Bool a_val -> Bool (not a_val)
     | _ -> failwith "invalid boolean for Not operation"
   )
                
| Function (a, b) -> Function (a, b)

| If (a, b, c) -> 
  let a_eval = eval_helper a ls in 
  (
   match a_eval with
     | Bool true -> eval_helper b ls
     | Bool false -> eval_helper c ls
     | _ -> failwith "invalid boolean for If operation"
  )

| Appl (a, b) -> 
  let b_eval = eval_helper b ls in
   (match a with
     | Var recid -> let result = find_recfun ls recid in
     (match result with
      | Some (var, body, _)->
        let sub_body = substitution var body b_eval in
          eval_helper sub_body ls
      | None -> failwith "invalid use of application rule")
    | _ -> (
  let a_eval = eval_helper a ls in 
  (
   match a_eval with 
     | Function (var, body) -> 
       let sub_body = substitution var body b_eval in
         eval_helper sub_body ls
     | _ -> failwith "invalid use of application rule"
  )
    ))
| Let (a, b, c) -> eval_helper (substitution a c (eval_helper b ls)) ls

| LetRec (a, b, c, d) -> let new_ls = (a, (b, c, d)) :: ls in
                           eval_helper d new_ls




let eval e = 
  let close_check = check_closed e [] in 
    if close_check then eval_helper e [] else failwith "Unbounded expression"

