let rec merge_int_lists l1 l2 = 
    match (l1,l2) with
    | [], [] -> []
    | [], _ -> l2
    | _, [] -> l1
    | (a1::t1), (a2::t2) -> if (a1 < a2) 
          then a1::(merge_int_lists t1 l2) 
          else  a2::(merge_int_lists l1 t2);;
  

let rec merge_any_lists l1 l2 cmp = 
    match (l1,l2) with
    | [], [] -> []
    | [], _ -> l2
    | _, [] -> l1
    | (a1::t1), (a2::t2) -> if (-1 == cmp a1 a2) 
        then a1::(merge_any_lists t1 l2 cmp) 
        else  a2::(merge_any_lists l1 t2 cmp);;
  

let merge_string_lists l1 l2 = merge_any_lists l1 l2 compare;;


type name = {fname: string; sname: string}

let merge_name_lists l1 l2 = merge_any_lists l1 l2 compare;;


let name_lists l1 l2 = 
  merge_any_lists l1 l2 (fun a1 a2 -> 
    let fn = compare a1.fname a2.fname in 
      if fn != 0 then fn else compare a1.sname a2.sname );;

let len lst =
  List.fold_left (fun acc x -> acc + 1) 0 lst;;

let forall predicate lst =
  List.fold_left (fun acc x -> acc && predicate x) true lst;;

let exists predicate lst=
  List.fold_left (fun acc x -> acc || predicate x) false lst;;


type typ =
  | TypI (* int *)
  | TypB (* bool *)
  | TypF of typ * typ (* function: T1 -> T2 *);;


type op = Add | Mult | Eq | Less;;

type expr =
  | ConstI of int
  | ConstB of bool
  | Var of string
  | BinOp of op * expr * expr (* Operation type, first subexpression, second subexpression *)
  | Let of string * expr * expr (* var name, expression to bind, let body *)
  | Fun of string * typ * expr (* parameter name, parameter type, function body *)
  | Call of string * expr (* function name, argument expression *)
  | If of expr * expr * expr (* Text expression, then branch, else branch *);;

exception UndefinedVariable of string;;
exception TypeError;;

let rec lookup v en =
  match en with 
    [] -> None
    | (name, tp)::t -> if name = v then Some tp else lookup v t;;
let rec typecheck e =
  let rec typecheck_env e env =
    match e with
    | ConstI _ -> TypI
    | ConstB _ -> TypB
    | If (e1, e2, _) ->  
        if (typecheck_env e1 env) = TypB 
        then (typecheck_env e2 env) 
        else raise (TypeError)
    | Var name -> 
        (match (lookup name env) with
          | Some tp -> tp
          | None -> raise (UndefinedVariable name))
    | BinOp (o, e1, e2) -> 
        (match o with
        | Add | Mult -> 
            if (typecheck_env e1 env) = TypI && (typecheck_env e2 env) = TypI 
            then TypI
            else raise (TypeError)
        | Eq | Less ->  
            if (typecheck_env e1 env) = TypI && (typecheck_env e2 env) = TypI 
            then TypB
            else raise (TypeError))
    | Call (n, exp) -> 
        (match (lookup n env) with
          | Some tp ->  
            (match tp with
                | TypF (arg, ret_type) -> 
                    if (typecheck_env exp env) = arg 
                    then ret_type 
                    else raise (TypeError)
                | _ -> raise (TypeError)) 
          | None -> raise (UndefinedVariable n))
    | Let (l, ex1, ex2) -> typecheck_env ex2 ((l, (typecheck_env ex1 env))::env)
    | Fun (n, tp, exp) -> TypF (tp, (typecheck_env exp ((n, tp)::env))) in 

    typecheck_env e [];;
        