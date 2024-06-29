open Tfbsrxast
open Tfbsrxpp

exception TypecheckerNotImplementedException;;
exception TypecheckerInvalidType;;
exception TypecheckerUnclosedVariable;;

(*
 * If you would like typechecking to be enabled by your interpreter by default,
 * then change the following value to true.  Whether or not typechecking is
 * enabled by default, you can explicitly enable it or disable it using
 * command-line arguments. 
 *) 
let typecheck_default_enabled = true;;

let rec typecheck_helper (e : expr) (gamma : (ident * fbtype) list) : fbtype = 

  let rec generalized_typechecker (sub_e : expr) (sub_gamma : (ident * fbtype) list) (expected_type : fbtype) : fbtype  = 
    let sub_e_type = typecheck_helper sub_e sub_gamma in 
       if ((equal_fbtype sub_e_type expected_type) || (equal_fbtype sub_e_type TBottom)) then sub_e_type else raise TypecheckerInvalidType in
  
  let rec general_doubled_typechecker (expr_a : expr) (expr_b : expr) (gamma :(ident * fbtype) list) (expected_type : fbtype) (return_type : fbtype) : fbtype = 
    let type_a = generalized_typechecker expr_a gamma expected_type in 
      let type_b = generalized_typechecker expr_b gamma expected_type in 
          if (equal_fbtype type_b TBottom || equal_fbtype type_a TBottom) then TBottom else return_type in

  match e with
  | Var id ->  let rec find_id_from_list (id_ls : (ident * fbtype) list) (identifier : ident) : fbtype = 
                  match id_ls with
                      | [] -> raise TypecheckerUnclosedVariable
                      | x :: xs -> let (x_id, x_type) = x in if (x_id = identifier) then x_type else find_id_from_list xs identifier in
                find_id_from_list gamma id

  | Int _ -> TInt

  | Bool _ -> TBool

  | Plus (a, b) -> general_doubled_typechecker a b gamma TInt TInt
  
  | Minus (a, b) -> general_doubled_typechecker a b gamma TInt TInt

  | Equal (a, b) -> general_doubled_typechecker a b gamma TInt TBool

  | And (a, b) -> general_doubled_typechecker a b gamma TBool TBool

  | Or (a, b) -> general_doubled_typechecker a b gamma TBool TBool

  | Not a -> generalized_typechecker a gamma TBool

  | If (a, b, c) -> let type_b = typecheck_helper b gamma in
                      let type_c = typecheck_helper c gamma in
                        let type_a = generalized_typechecker a gamma TBool in
                          if (equal_fbtype type_a TBottom) then type_a 
                          else 
                            if (equal_fbtype type_b type_c) then type_b 
                            else 
                              if (equal_fbtype type_b TBottom) then type_c
                              else
                                if (equal_fbtype type_c TBottom) then type_b
                                else raise TypecheckerInvalidType
  
  | Function (a, b, c) -> let new_gamma = (a, b) :: gamma in TArrow (b, (typecheck_helper c new_gamma))

  | Appl (a, b) -> let fun_type = typecheck_helper a gamma in
                      let rec substitutable_param (input_type :fbtype) (param_type : fbtype) : bool = (match param_type with
                      | TBottom -> true
                      | TRef param_deref -> (match input_type with
                                             | TRef input_deref -> substitutable_param input_deref param_deref
                                             | _ -> raise TypecheckerInvalidType)
                      | TRec param_list -> let rec find_query (ls : (label * fbtype) list) (query : label) : fbtype = 
                                               (match ls with
                                                | [] -> raise TypecheckerUnclosedVariable
                                                | x :: xs -> let (lab, fb_type) = x in if (lab = query) then fb_type else (find_query xs query)) in
                                            (match input_type with
                                              | TRec input_list -> let rec compare_rec (i_ls : (label * fbtype) list) (p_ls : (label * fbtype) list) : bool = 
                                                                          (match i_ls with
                                                                          | [] -> true
                                                                          | x :: xs -> let (i_lab, i_type) = x in 
                                                                                          let p_type = find_query p_ls i_lab in
                                                                                             ((equal_fbtype p_type TBottom) || (equal_fbtype p_type i_type)) && (compare_rec xs p_ls)) in
                                                                      compare_rec input_list param_list
                                              | _ -> raise TypecheckerInvalidType)
                      | _ -> equal_fbtype input_type param_type) in
                     (match fun_type with
                      | TArrow (input, output) -> let param_type = typecheck_helper b gamma in
                            if (substitutable_param input param_type) then output 
                            else raise TypecheckerInvalidType
                      | TBottom -> fun_type
                      | _ -> raise TypecheckerInvalidType)
  
  | LetRec (a, b, c, d, e, f) -> typecheck_helper f ((a, TArrow (c, e)) :: gamma)
  
  | Record rec_ls -> let rec recur_type_accumulator (rec_list : (label * expr) list) (cur_types : (label * fbtype) list) : (label * fbtype) list =
                        (match rec_list with
                        | [] -> cur_types
                        | x :: xs -> let (cur_lab, cur_expr) = x in 
                                        (recur_type_accumulator xs ((cur_lab, (typecheck_helper cur_expr gamma)) :: cur_types))) in
                        TRec (recur_type_accumulator rec_ls [])
  
  | Select (a, b) -> let record_type = typecheck_helper b gamma in 
                        (match record_type with
                        | TRec rec_type_ls -> (let rec recur_type_finder (rec_ls : (label * fbtype) list) (query : label) : fbtype =
                                (match rec_ls with
                                    | [] -> raise TypecheckerUnclosedVariable
                                    | x :: xs -> let (cur_lab, cur_type) = x in 
                                         if (cur_lab = query) then cur_type else (recur_type_finder xs query)) in
                              recur_type_finder rec_type_ls a)
                        | TBottom -> record_type
                        | _ -> raise TypecheckerInvalidType)
  
  | Raise (a, b, c) -> let raise_type = typecheck_helper c gamma in
                             if (equal_fbtype b raise_type) || (raise_type = TBottom) then TBottom else raise TypecheckerInvalidType

  | Try (a, b, c, d, e) -> let try_type = typecheck_helper a gamma in
                             let with_type = typecheck_helper e ((c, d) :: gamma) in
                             if (equal_fbtype try_type with_type || equal_fbtype try_type TBottom) then with_type else raise TypecheckerInvalidType

  | Ref a -> TRef (typecheck_helper a gamma)
  
  | Set (a, b) -> let a_type = typecheck_helper a gamma in
                    let b_type = typecheck_helper b gamma in 
                      (match a_type with
                      | TBottom -> TBottom
                      | TRef a_deref -> if (equal_fbtype a_deref b_type) then b_type else raise TypecheckerInvalidType
                      | _ -> raise TypecheckerInvalidType)
  
  | Get a -> let a_type = typecheck_helper a gamma in
                (match a_type with
                      | TBottom -> TBottom
                      | TRef a_deref -> a_deref
                      | _ -> raise TypecheckerInvalidType)

  | _ -> raise TypecheckerNotImplementedException;;

(*
 * Replace this with your typechecker code.  Your code should not throw the
 * following exception; if you need to raise an exception, create your own
 * exception type here.
 *) 
let typecheck e = typecheck_helper e [];;
