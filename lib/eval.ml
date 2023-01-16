open Ast

let update_env env k v =
  let env = List.remove_assoc k env in
  (k, v) :: env

let eval_sample d env env_alt =
  let support = Distribution.get_support d in
  let vlist = Array.to_list support.values in
  let plist = Array.to_list support.probs in
  let v = List.hd vlist in
  let alt = List.combine vlist plist in
  (v, env, alt, env_alt)

let rec op_list_1 op el l =
  match l with [] -> [] | x :: q -> op el x :: op_list_1 op el q

let rec op_list op l1 l2 =
  match (l1, l2) with
  | [], _ -> []
  | _, [] -> []
  | x :: q, l -> op_list_1 op x l @ op_list op q l

let rec op_duplicate_1 op test x l =
  match l with
  | [] -> (x, [])
  | h :: t ->
      if test x h then
        let x = op x h in
        op_duplicate_1 op test x t
      else op_duplicate_1 op test x t

let rec split l test =
  match l with
  | [] -> ([], [])
  | x :: q ->
      let l1, l2 = split q test in
      if test x then (x :: l1, l2) else (l1, x :: l2)

let alt_guard b l1 l2 =
  let b1, b2 = split b (fun (x, _) -> x > 0.) in
  op_list (fun (_, p1) (x, p2) -> (x, p1 *. p2)) b1 l1
  @ op_list (fun (_, p1) (x, p2) -> (x, p1 *. p2)) b2 l2

let rec op_duplicate op test l =
  match l with
  | [] -> []
  | x :: q ->
      let x, t = op_duplicate_1 op test x q in
      (x :: t) @ op_duplicate op test q
let third (_,_,x,_) =x
let rec eval_expr env e env_alt =
  match e with
  | Const f -> (f, env, [(f,1.)],[])
  | Var x ->
      let v = List.assoc x env in
      let alt = List.assoc x env_alt in
      (v, env, alt, env_alt)
  | Add (e1, e2) when e1 = e2 ->
      let v1, _, alt, _ = eval_expr env e1 env_alt in
      let alt = List.map (fun (v, p) -> (v +. v, p)) alt in
      (v1 +. v1, env, alt, env_alt)
  | Add (e1, e2) when e1 <> e2 ->
      let v1, _, alt1, _ = eval_expr env e1 env_alt in
      let v2, _, alt2, _ = eval_expr env e2 env_alt in
      let op (v, p) (v2, p2) = (v +. v2, p *. p2) in
      let alt = op_list op alt1 alt2 in
      (v1 +. v2, env, alt, env_alt)
  | Sub (e1, e2) when e1 = e2 ->
      let v1, _, alt, _ = eval_expr env e1 env_alt in
      let alt = List.map (fun (v, p) -> (v -. v, p)) alt in
      (v1 +. v1, env, alt, env_alt)
  | Sub (e1, e2) ->
      let v1, _, alt1, _ = eval_expr env e1 env_alt in
      let v2, _, alt2, _ = eval_expr env e2 env_alt in
      let op (v, p) (v2, p2) = (v -. v2, p *. p2) in
      let alt = op_list op alt1 alt2 in
      (v1 -. v2, env, alt, env_alt)
  | IfThenElse (e1, e2, e3) ->
      let b, _, altb, _ = eval_expr env e1 env_alt in
      let v2, _, alt2, _ = eval_expr env e2 env_alt in
      let v3, _, alt3, _ = eval_expr env e3 env_alt in
      let alt = alt_guard altb alt2 alt3 in
      ((if b > 0. then v2 else v3), env, alt, env_alt)
  | LetIn (x, e1, e2) ->
      let v, env, alt, env_alt = eval_expr env e1 env_alt in
      let env = update_env env x v in
      let env_alt = update_env env_alt x alt in
      eval_expr env e2 env_alt
  | Sample d -> (

      match d with
      | Bernoulli e -> let  pval, _, alt, _ = eval_expr env e env_alt in 
                       let alt = List.fold_left (fun  a -> fun (x,p) ->List.append  a  [(third (eval_sample (Distribution.bernoulli ~p:x) env env_alt),p)] ) [] alt in
                       let alt = List.fold_left (fun  a -> fun (l,pl) -> List.append a (List.fold_left (fun b -> fun (x,p) -> List.append b [(x, pl *.p )] ) [] l ) ) [] alt in
                       List.iter (fun (v,p) -> Printf.printf "v : %f ,p: %f ;" v p) alt ; 
                       print_endline "ici";
                       pval,env,alt,env_alt
                      
      | UniformD (a, b) ->
          eval_sample (Distribution.uniform_discrete ~a ~b) env env_alt)
  | _ -> failwith "nyi"

let infer m =
  match m with
  | Let (_, e) ->
      let _, _, alt, _ = eval_expr [] e [] in
      let vlist, plist = List.split alt in
      Distribution.support_p ~values:(Array.of_list vlist)
        ~probs:(Array.of_list plist)

let eval_prog p = match p with Infer m -> infer m
