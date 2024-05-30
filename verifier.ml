(* 값 평가 함수 *)
let rec eval_value env v =
  match v with
  | E_int n -> n
  | E_bool b -> if b then 1 else 0 
  | E_var x -> List.assoc x env
  | E_arr_idx (x, e) ->
     let idx = eval_expr env e in
     let arr = List.assoc x env in
     arr.(idx)
  | E_arr_modify (x, e1, e2) ->
     let idx = eval_expr env e1 in
     let value = eval_expr env e2 in
     let arr = List.assoc x env in
     let new_arr = Array.copy arr in
     new_arr.(idx) <- value;
     new_arr
  | E_binop (op, e1, e2) ->
     let v1 = eval_expr env e1 in
     let v2 = eval_expr env e2 in
     (match op with
      | Add -> v1 + v2
      | Sub -> v1 - v2
      | Mul -> v1 * v2
      | Div -> v1 / v2)
  | E_unop (Not, e) -> 
     let v = eval_expr env e in
     if v = 0 then 1 else 0
  | E_unop (Neg, e) ->
     let v = eval_expr env e in
     -v
  | E_len x ->
     let arr = List.assoc x env in
     Array.length arr
  | E_cmp (op, e1, e2) ->
     let v1 = eval_expr env e1 in
     let v2 = eval_expr env e2 in
     (match op with
      | Eq -> if v1 = v2 then 1 else 0
      | Neq -> if v1 <> v2 then 1 else 0
      | Lt -> if v1 < v2 then 1 else 0
      | Gt -> if v1 > v2 then 1 else 0
      | Le -> if v1 <= v2 then 1 else 0
      | Ge -> if v1 >= v2 then 1 else 0)

(* 논리식 평가 함수 *)
and eval_formula env f =
  match f with
  | F_value e -> eval_value env e <> 0
  | F_order (es1, es2) ->
     let rec aux es1 es2 =
       match es1, es2 with
       | [], [] -> true
       | e1::r1, e2::r2 -> eval_value env e1 < eval_value env e2 && aux r1 r2
       | _ -> false
     in aux es1 es2
  | F_connective (Not, f1) -> not (eval_formula env f1)
  | F_connective (And, fml_list) -> List.for_all (eval_formula env) fml_list  
  | F_connective (Or, fml_list) -> List.exists (eval_formula env) fml_list
  | F_biconnective (Imply, f1, f2) -> not (eval_formula env f1) || eval_formula env f2
  | F_biconnective (Iff, f1, f2) -> (eval_formula env f1) = (eval_formula env f2)
  | F_quantifier (Forall, x, Some t, f1) ->
     let values =
       match t with
       | T_int -> List.init 100 (fun i -> i) (* Assuming int range 0-99 *)
       | T_bool -> [0; 1]
       | T_arr t1 ->
          let rec gen_arrays n =
            if n = 0 then [[||]]
            else List.concat (List.map (fun arr -> [arr; Array.append arr [|0|]; Array.append arr [|1|]]) (gen_arrays (n-1)))
          in gen_arrays 3 (* Assuming array length up to 3 *)
     in
     List.for_all (fun v -> eval_formula ((x, v)::env) f1) values
  | F_quantifier (Exists, x, Some t, f1) ->
     let values =
       match t with
       | T_int -> List.init 100 (fun i -> i)
       | T_bool -> [0; 1]  
       | T_arr t1 ->
          let rec gen_arrays n =
            if n = 0 then [[||]]
            else List.concat (List.map (fun arr -> [arr; Array.append arr [|0|]; Array.append arr [|1|]]) (gen_arrays (n-1)))
          in gen_arrays 3
     in 
     List.exists (fun v -> eval_formula ((x, v)::env) f1) values
  | F_quantifier (Forall, x, None, f1) ->
     let values = [0; 1] in
     List.for_all (fun v -> eval_formula ((x, v)::env) f1) values
  | F_quantifier (Exists, x, None, f1) ->
     let values = [0; 1] in 
     List.exists (fun v -> eval_formula ((x, v)::env) f1) values     
  | F_array_prop (Sorted, x, start, end_idx) ->
     let arr = List.assoc x env in
     let start_idx = eval_value env start in
     let end_idx = eval_value env end_idx in
     let rec is_sorted l u =
       if l >= u then true
       else if l = u - 1 then true  
       else arr.(l) <= arr.(l+1) && is_sorted (l+1) u
     in is_sorted start_idx end_idx        
  | F_array_prop (Partitioned, x, start1, end1, start2, end2) ->
     let arr = List.assoc x env in
     let start1_idx = eval_value env start1 in
     let end1_idx = eval_value env end1 in
     let start2_idx = eval_value env start2 in  
     let end2_idx = eval_value env end2 in
     let rec is_partitioned l1 u1 l2 u2 =
       if l1 > u1 then l2 > u2
       else if l2 > u2 then false
       else arr.(u1) <= arr.(l2) && is_partitioned l1 (u1-1) l2 u2
     in is_partitioned start1_idx end1_idx start2_idx end2_idx
             
(* 문장 실행 함수 *)  
and execute_stmt env s =
  match s with
  | S_nop -> env
  | S_assign (x, e) ->
     let v = eval_value env e in
     (x, v) :: List.remove_assoc x env
  | S_arr_assign (x, idx, e) ->
     let i = eval_value env idx in
     let v = eval_value env e in
     let arr = List.assoc x env in
     let new_arr = Array.copy arr in
     new_arr.(i) <- v;
     (x, new_arr) :: List.remove_assoc x env      
  | S_sequence stmts -> List.fold_left execute_stmt env stmts
  | S_branch (e, s1, s2) ->
     if eval_value env e <> 0 then execute_stmt env s1 else execute_stmt env s2
  | S_loop (inv, cond, body) ->
     let rec loop env
let default_value = function
  | T_int -> 0
  | T_bool -> false
  | T_arr t -> [||]
