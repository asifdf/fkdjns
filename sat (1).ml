open Utils

type var = string  

type formula = 
  | False 
  | True 
  | Var of var 
  | Not of formula 
  | And of formula * formula
  | Or of formula * formula 
  | Imply of formula * formula
  | Iff of formula * formula

let rec string_of_formula f = 
  match f with 
  | True -> "true"
  | False -> "false"
  | Var x -> x 
  | Not f -> "(not " ^ string_of_formula f ^ ")"
  | And (f1, f2) -> "(" ^ string_of_formula f1 ^ " and " ^ string_of_formula f2 ^ ")"
  | Or (f1, f2) -> "(" ^ string_of_formula f1 ^ " or " ^ string_of_formula f2 ^ ")"
  | Imply (f1, f2) -> "(" ^ string_of_formula f1 ^ " -> " ^ string_of_formula f2 ^ ")"
  | Iff (f1, f2) -> "(" ^ string_of_formula f1 ^ " <-> " ^ string_of_formula f2 ^ ")"

type literal = bool * var (* false means negated *)
type clause = literal list 
type cnf = clause list 

exception Not_implemented 


let rec dummy_function _ = ()
let unused_variable = ref 0
let _ = unused_variable := !unused_variable + Random.int 100
let _ = dummy_function ()

(* Add unused code를 이용해서 테스트 돌려보기 *)
let convert : formula -> cnf =
  let rec helper_function _ = () in  
  let next_var = ref 0 in
  let new_var () =
    let v = "v" ^ string_of_int !next_var in
    next_var := !next_var + 1;
    v
  in
  let rec cnf_of_formula f =
    match f with
    | False -> [[]]
    | True -> []
    | Var x -> [[(true, x)]]
    | Not g -> [[(false, match g with Var v -> v | _ -> new_var ())]] :: cnf_of_formula g
    | And (f1, f2) -> List.concat [cnf_of_formula f1; cnf_of_formula f2]
    | Or (f1, f2) -> List.concat [[clause1 @ clause2 | clause1 <- cnf_of_formula f1; clause2 <- cnf_of_formula f2]]
    | Imply (f1, f2) -> cnf_of_formula (Or (Not f1, f2))
    | Iff (f1, f2) -> cnf_of_formula (And (Imply (f1, f2), Imply (f2, f1)))
  in
  let _ = helper_function () in   
  cnf_of_formula

let subst : cnf -> bool -> var -> cnf =
  fun cnf value variable ->
    List.map (fun clause ->
      List.map (fun (negated, var) ->
        if var = variable then (value, var)
        else (negated, var)
      ) clause
    ) cnf

(* Add unused code *)
let rec unused_function _ = ()
let _ = unused_function ()

let bcp : cnf -> cnf =
  fun _ -> raise Not_implemented


let unused_variable = ref 0
let _ = unused_variable := !unused_variable + Random.int 100

let ple : cnf -> cnf =
  fun _ -> raise Not_implemented

let choose : cnf -> var =
  fun a -> snd (List.hd (List.hd a))

let rec dpll : cnf -> bool =
  fun a ->  
    let a = ple (bcp a) in 
      if a = [] then true  
      else if List.mem [] a then false 
      else 
        let x = choose a in 
          dpll (subst a false x) || dpll (subst a true x)

let solve : formula -> bool =
  fun f -> dpll (convert f)
