open Fbrxast

exception NotClosed
[@@@warning "-27"]

let fbLabelNotFound : expr = Raise("#LabelNotFound", Int(0))
let fbTypeMismatch : expr  = Raise("#TypeMismatch", Int(0))


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
    | Record l -> (match l with 
                      | [] -> true
                      | x :: xs -> let (lab, e) = x in
                                       check_closed e ls && check_closed (Record xs) ls)
    | Select (a, b) -> check_closed b ls
    | Append (a, b) -> check_closed a ls && check_closed b ls
    | Raise (a, b) -> check_closed b ls
    | Try (a, b, c, d) -> check_closed a ls && (let new_ls = c :: ls in check_closed d new_ls)
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
    | LetRec (a, b, c, d) -> if (a = var) then LetRec (a, b, c, d) else 
      (if (b = var) then LetRec (a, b, c, (substitution var d value)) else LetRec (a, b, (substitution var c value), (substitution var d value)))
    | Record l -> (match l with
                  | [] -> Record []
                  | x :: xs -> let (lab, e) = x in
                                let e_sub = substitution var e value in
                                   match (substitution var (Record xs) value) with 
                                      | Record xs_sub -> Record ((lab, e_sub) :: xs_sub)
                                      | _ -> failwith "invalid record format"
    )
    | Select (a, b) -> Select (a, substitution var b value)
    | Append (a, b) -> Append(substitution var a value, substitution var b value)
    | Raise (a, b) -> Raise (a, substitution var b value)
    | Try (a, b, c, d) -> if c = var then Try (substitution var a value, b, c, d) else Try (substitution var a value, b, c, substitution var d value)
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

  |String str_val -> String str_val

  | Plus (a, b)  -> 
   let a_eval = eval_helper a ls in 
     let b_eval = eval_helper b ls in
   (
   match a_eval with
     | Int a_val -> (match b_eval with
       | Int b_val -> Int (a_val + b_val)
       | Raise (b_ex, b_expr) -> b_eval
       | _ -> fbTypeMismatch
     )
     | Raise (a_ex, a_expr) -> a_eval
     | _ -> fbTypeMismatch
   )

  | Minus (a, b) -> 
 let a_eval = eval_helper a ls in 
 let b_eval = eval_helper b ls in
(
match a_eval with
 | Int a_val -> (match b_eval with
   | Int b_val -> Int (a_val - b_val)
   | Raise (b_ex, b_expr) -> b_eval
   | _ -> fbTypeMismatch
 )
 | Raise (a_ex, a_expr) -> a_eval
 | _ -> fbTypeMismatch
)

  | Equal (a, b) -> 
  let a_eval = eval_helper a ls in 
  let b_eval = eval_helper b ls in
(
match a_eval with
 | Int a_val -> (match b_eval with
   | Int b_val -> Bool (a_val = b_val)
   | Raise (b_ex, b_expr) -> b_eval
   | _ -> fbTypeMismatch
 )
 | String a_str -> (match b_eval with
    | String b_str -> Bool (a_str = b_str)
    | Raise (b_ex, b_expr) -> b_eval
    | _ -> fbTypeMismatch)
 | Raise (a_ex, a_expr) -> a_eval
 | _ -> fbTypeMismatch
)

  | And (a, b) -> 
  let a_eval = eval_helper a ls in 
  let b_eval = eval_helper b ls in
(
match a_eval with
 | Bool a_val -> (match b_eval with
   | Bool b_val -> Bool (a_val && b_val)
   | Raise (b_ex, b_expr) -> b_eval
   | _ -> fbTypeMismatch
 )
 | Raise (a_ex, a_expr) -> a_eval
 | _ -> fbTypeMismatch
)

  | Or (a, b) -> 
  let a_eval = eval_helper a ls in 
  let b_eval = eval_helper b ls in
(
match a_eval with
 | Bool a_val -> (match b_eval with
   | Bool b_val -> Bool (a_val || b_val)
   | Raise (b_ex, b_expr) -> b_eval
   | _ -> fbTypeMismatch
 )
 | Raise (a_ex, a_expr) -> a_eval
 | _ -> fbTypeMismatch
)

  | Not a -> 
  let a_eval = eval_helper a ls in 
 (
   match a_eval with
     | Bool a_val -> Bool (not a_val)
     | Raise (a_ex, a_expr) -> a_eval
     | _ -> fbTypeMismatch
   )
                
  | Function (a, b) -> Function (a, b)

  | If (a, b, c) -> 
  let a_eval = eval_helper a ls in 
  (
   match a_eval with
     | Bool true -> eval_helper b ls
     | Bool false -> eval_helper c ls
     | Raise (if_ex, if_expr) -> a_eval
     | _ -> fbTypeMismatch
  )

  | Appl (a, b) -> 
  let b_eval = eval_helper b ls in
  (
    match b_eval with
    | Raise (b_exn, b_expr) -> b_eval
    | _ ->
   (match a with
     | Var recid -> let result = find_recfun ls recid in
     (match result with
      | Some (var, body, _)->
        let sub_body = substitution var body b_eval in
          eval_helper sub_body ls
      | None -> fbTypeMismatch)
    | _ -> (
  let a_eval = eval_helper a ls in 
  (
   match a_eval with 
     | Function (var, body) -> 
       let sub_body = substitution var body b_eval in
         eval_helper sub_body ls
     | Raise (a_ex, a_expr) -> a_eval
     | _ -> fbTypeMismatch
  )
    )
    )
  )
  | Let (a, b, c) -> let val_eval = eval_helper b ls in
                      (match val_eval with
                      | Raise (val_exn, val_expr) -> val_eval
                      | _ -> eval_helper (substitution a c val_eval) ls
                      )

  | LetRec (a, b, c, d) -> let new_ls = (a, (b, c, d)) :: ls in
                           eval_helper d new_ls
  
  | Record l -> 
    (match l with
      | [] -> Record l
      | x :: xs -> let (lab, e) = x in
        let e_eval = eval_helper e ls in
         ( match e_eval with
           | Raise (e_ex, e_expr) -> e_eval
           | _ -> let xs_eval = eval_helper (Record xs) ls in
          (match xs_eval with
            | Record xs_eval -> Record ((lab, e_eval) :: xs_eval)
            | Raise (e_ex, e_expr) -> xs_eval
            | _ -> fbTypeMismatch
          )
         )
  )

  | Select (a, b) -> 
    let b_eval = eval_helper b ls in
      (match b_eval with
        | Record b_rec -> 
          let rec find_lab_helper (q : (label * expr) list) (k : label) : expr = (match q with
            | [] -> fbLabelNotFound
              | x :: xs -> let (lab, e) = x in 
                if (lab = k) then e else (find_lab_helper xs k)
            ) in
          find_lab_helper b_rec a
        | Raise (b_ex, b_expr) -> b_eval
        | _ -> fbTypeMismatch
      )
  
  | Append (a, b) -> 
    let a_eval = eval_helper a ls in
     let b_eval = eval_helper b ls in
     let rec record_append_helper_helper (a_lab : label) (a_expr : expr) (b_ls : (label * expr) list) : (label * expr) list = 
      match b_ls with
        | [] -> [(a_lab, a_expr)]
        | x :: xs -> let (lab, e) = x in 
                       if a_lab = lab then b_ls else x :: (record_append_helper_helper a_lab a_expr xs) in
        let rec record_append_helper (a_ls : (label * expr) list) (b_ls : (label * expr) list) : (label * expr) list = 
              match a_ls with
                | [] -> b_ls
                | x :: xs -> let (lab, e) = x in 
                    record_append_helper xs (record_append_helper_helper lab e b_ls) in
        (match a_eval with
          | String a_str -> (match b_eval with
                                | String b_str -> String (a_str ^ b_str)
                                | Raise (b_exn, b_expr) -> b_eval
                                | _ -> fbTypeMismatch)
          | Record a_rec -> (match b_eval with
                                | Record b_rec -> Record (record_append_helper a_rec b_rec)
                                | Raise (b_exn, b_expr) -> b_eval
                                | _ -> fbTypeMismatch)
          | Raise (a_ex, a_expr) -> a_eval
          | _ -> fbTypeMismatch)
    
  | Try (a, b, c, d) -> 
    let a_eval = eval_helper a ls in
      (match a_eval with
        | Raise (a_exn, a_e) -> if (a_exn = b) then eval_helper (substitution c d a_e) ls else Raise (a_exn, a_e)
        | _ -> a_eval
      )                     
  
  | Raise (a, b) -> 
    let b_eval = eval_helper b ls in
      match b_eval with
      | Raise (b_exn, b_excep) -> b_eval
      | _ -> Raise (a, b_eval)

let eval e = 
  let close_check = check_closed e [] in 
    if close_check then eval_helper e [] else raise NotClosed


