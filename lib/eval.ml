open Ast

(* Compute all possible value after a sample*)
let eval_sample d env env_alt =
  let support = Distribution.get_support d in
  let vlist = Array.to_list support.values in
  let plist = Array.to_list support.probs in
  let v = List.hd vlist in
  let alt = List.combine vlist plist in
  (v, env, alt, env_alt)

(* Functions to compute an operation on multiple list. *)
let rec op_list_1 op el l =
  match l with [] -> [] | x :: q -> op el x :: op_list_1 op el q

let rec op_list op l1 l2 =
  match (l1, l2) with
  | [], _ -> []
  | _, [] -> []
  | x :: q, l -> op_list_1 op x l @ op_list op q l

let rec op_list_2 op el l1 l2 =
  match (l1, l2) with
  | [], [] -> []
  | _, [] | [], _ -> []
  | x :: q, y :: p -> op el x y :: op_list_2 op el q p

let rec op_list_3 op l1 l2 l3 =
  match (l1, l2, l3) with
  | _, [], _ -> []
  | [], _, _ -> []
  | _, _, [] -> []
  | x :: q, l1, l2 -> op_list_2 op x l1 l2 @ op_list_3 op q l1 l2

let rec op_duplicate_1 op test x l =
  match l with
  | [] -> (x, [])
  | h :: t ->
      if test x h then
        let x = op x h in
        op_duplicate_1 op test x t
      else op_duplicate_1 op test x t

let rec op_duplicate op test l =
  match l with
  | [] -> []
  | x :: q ->
      let x, t = op_duplicate_1 op test x q in
      (x :: t) @ op_duplicate op test q

let rec split l test =
  match l with
  | [] -> ([], [])
  | x :: q ->
      let l1, l2 = split q test in
      if test x then (x :: l1, l2) else (l1, x :: l2)

(* Compute all possible values w.r.t the possibles values of an expr that appear in an If(expr)*)
let alt_guard b l1 l2 =
  let b1, b2 = split b (fun (x, _) -> x > 0.) in
  op_list (fun (_, p1) (x, p2) -> (x, p1 *. p2)) b1 l1
  @ op_list (fun (_, p1) (x, p2) -> (x, p1 *. p2)) b2 l2

let third (_, _, x, _) = x

let rec remove_duplicate_bis x l =
  match l with
  | y :: q ->
      if y = x then remove_duplicate_bis x q else y :: remove_duplicate_bis x q
  | [] -> []

let rec remove_duplicate l =
  match l with
  | y :: q -> y :: remove_duplicate (remove_duplicate_bis y q)
  | [] -> []

let rec remove_duplicate_bis' (x, p) l =
  match l with
  | (y, py) :: q ->
      if y = x then remove_duplicate_bis' (x, p) q
      else (y, py) :: remove_duplicate_bis' (x, p) q
  | [] -> []

let rec remove_duplicate_2 l =
  match l with
  | y :: q -> y :: remove_duplicate_2 (remove_duplicate_bis' y q)
  | [] -> []

(* Update an environnemnet *)
let update_env env k v =
  let env = remove_duplicate_2 env in
  let env = List.remove_assoc k env in
  (k, v) :: env

let update_env_list env_alt x alt =
  if alt = [] then env_alt
  else
    match env_alt with
    | [] -> List.map (fun a -> [ (x, a) ]) alt
    | _ ->
        let env_alt = List.map (fun e -> remove_duplicate_2 e) env_alt in
        let env_alt = List.map (fun e -> List.remove_assoc x e) env_alt in
        let env_alt = List.mapi (fun i e -> (x, List.nth alt i) :: e) env_alt in

        env_alt

(* Compute intersection of list*)
let intersection_list l1 l2 = List.filter (fun x -> List.exists (( = ) x) l2) l1

let intersection_list_2 l1 l2 =
  List.filter (fun (x, _) -> List.exists (fun (y, _) -> x = y) l2) l1

(* Function that find all assume in the prog and compute the list of condition. It implies that each assume has a global scope*)
let rec find_assume e =
  match e with
  | Const _ -> []
  | Var _ -> []
  | Add (e1, e2) | Sub (e1, e2) ->
      let clist1 = find_assume e1 in
      let clist2 = find_assume e2 in
      let clist = List.append clist1 clist2 in
      clist
  | IfThenElse (e1, e2, e3) ->
      let clist1 = find_assume e1 in
      let clist2 = find_assume e2 in
      let clist3 = find_assume e3 in
      let clist = clist1 @ clist2 @ clist3 in
      clist
  | LetIn (_, e1, e2) ->
      let clist1 = find_assume e1 in
      let clist2 = find_assume e2 in
      clist1 @ clist2
  | Sample _ -> []
  | Assume cond -> [ cond ]
  | _ -> failwith "nyi"

(*| Sub of expr * expr
  | IfThenElse of expr * expr * expr
  | LetIn of var * expr * expr
  | Sample of dist
  | Assume of cond
  | Observe of expr
*)

(* Operaiton on env*)
let union_env env1 env2 =
  let env =
    List.map
      (fun (x, l) ->
        let l2 =
          (fun y ->
            match List.assoc_opt y env2 with Some li -> li | None -> [])
            x
        in
        (x, intersection_list_2 l l2))
      env1
  in
  let env =
    List.fold_left
      (fun a (x, l) -> if List.assoc_opt x env = None then (x, l) :: a else a)
      env env2
  in

  env

let intersection_env env1 env2 =
  List.filter
    (fun (_, l) -> l <> [])
    (List.map
       (fun (x, l) ->
         let l2 =
           (fun y ->
             match List.assoc_opt y env2 with Some li -> li | None -> [])
             x
         in
         if l2 <> [] then (x, intersection_list_2 l l2) else (x, []))
       env1)

let complement env env_alt =
  List.filter
    (fun (_, l) -> l <> [])
    (List.map
       (fun (x, l) ->
         let l2 =
           (fun y ->
             match List.assoc_opt y env_alt with Some li -> li | None -> [])
             x
         in
         if l2 <> [] then
           let l2 =
             List.filter (fun (v, p) -> not (List.exists (( = ) (v, p)) l2)) l
           in
           (x, l2)
         else (x, []))
       env)

(* Evaluation of a condition. It should  return a new env.
   Here env_alt. Is a list of env i.e (var *(float *float ) list) list ) . The reason to that is Or. Without or we can do with just a simple env.
   Or implie that multiple setting of the variable are allowed in the prog. That's why we need multiple environnement.
*)
let rec eval_cond c env_alt =
  match c with
  | Or (c1, c2) ->
      let env1 = eval_cond c1 env_alt in
      let env2 = eval_cond c2 env_alt in
      env1 @ env2
  | Not c ->
      let env = eval_cond c env_alt in
      let op e1 e2 = complement e1 e2 in
      op_list op env env_alt
  | And (c1, c2) ->
      let env1 = eval_cond c1 env_alt in
      let env2 = eval_cond c2 env_alt in
      let op e1 e2 = intersection_env e1 e2 in
      op_list op env1 env2
  | False -> []
  | True -> env_alt
  | Eq (x, v) ->
      let x_alt =
        List.map
          (fun e ->
            (fun l -> match l with None -> [] | Some l -> l)
              (List.assoc_opt x e))
          env_alt
      in
      let x_alt =
        List.map (fun a -> List.filter (fun (w, _) -> v = w) a) x_alt
      in
      update_env_list env_alt x x_alt

let eval_cond_list clist env_alt =
  let rec aux clist env_alt =
    match clist with [] -> env_alt | c :: q -> aux q (eval_cond c env_alt)
  in
  aux clist env_alt

(* Evaluation of an "expression" it return (value,env,alt,env_alt).
   Value and env are useless but can be usefull to implement non exact inference algorithm.
   Alt and env_alt are the keys elements to exact inference. Alt is the a list of (list of possible value with there correspond probability).
   As explaine above, it is a list of list because of the "OR". Env_alt is as explained in eval_cond
*)
let rec eval_expr env e clist env_alt =
  match e with
  | Const f -> (f, env, [ [ (f, 1.) ] ], [])
  | Var x ->
      let env_alt = eval_cond_list clist env_alt in
      let alt = List.map (fun e -> List.assoc x e) env_alt in
      (0., env, alt, env_alt)
  | Add (e1, e2) when e1 = e2 ->
      let env_alt = eval_cond_list clist env_alt in
      let v1, _, alt, _ = eval_expr env e1 clist env_alt in
      let alt =
        List.map (fun la -> List.map (fun (v, p) -> (v +. v, p)) la) alt
      in
      (v1 +. v1, env, alt, env_alt)
  | Add (e1, e2) when e1 <> e2 ->
      let env_alt = eval_cond_list clist env_alt in
      let v1, _, alt1, _ = eval_expr env e1 clist env_alt in
      let v2, _, alt2, _ = eval_expr env e2 clist env_alt in
      let op1 (v, p) (v2, p2) = (v +. v2, p *. p2) in
      let op l1 l2 = op_list op1 l1 l2 in
      let alt = op_list op alt1 alt2 in

      (v1 +. v2, env, alt, env_alt)
  | Sub (e1, e2) when e1 = e2 ->
      let env_alt = eval_cond_list clist env_alt in
      let v1, _, alt, _ = eval_expr env e1 clist env_alt in
      let alt =
        List.map (fun la -> List.map (fun (v, p) -> (v +. v, p)) la) alt
      in
      (v1 +. v1, env, alt, env_alt)
  | Sub (e1, e2) when e1 <> e2 ->
      let env_alt = eval_cond_list clist env_alt in
      let v1, _, alt1, _ = eval_expr env e1 clist env_alt in
      let v2, _, alt2, _ = eval_expr env e2 clist env_alt in
      let op1 (v, p) (v2, p2) = (v +. v2, p *. p2) in
      let op l1 l2 = op_list op1 l1 l2 in
      let alt = op_list op alt1 alt2 in

      (v1 +. v2, env, alt, env_alt)
  | IfThenElse (e1, e2, e3) ->
      let b, _, altb, env_alt1 = eval_expr env e1 clist env_alt in
      let v2, _, alt2, _ = eval_expr env e2 clist env_alt1 in
      let v3, _, alt3, _ = eval_expr env e3 clist env_alt1 in
      let op l1 l2 l3 = alt_guard l1 l2 l3 in
      let alt = op_list_3 op altb alt2 alt3 in
      let env_alt = eval_cond_list clist env_alt in
      ((if b > 0. then v2 else v3), env, alt, env_alt)
  | LetIn (x, e1, e2) ->
      let _, env, alt, env_alt = eval_expr env e1 clist env_alt in
      let env_alt = update_env_list env_alt x alt in
      let env_alt = eval_cond_list clist env_alt in

      let v, env, alt, env_alt = eval_expr env e2 clist env_alt in

      let env_alt = eval_cond_list clist env_alt in
      (v, env, alt, env_alt)
  | Sample d -> (
      match d with
      | Bernoulli e ->
          let pval, _, alt, _ = eval_expr env e clist env_alt in
          let alt =
            List.map
              (fun a ->
                List.fold_left
                  (fun a (x, p) ->
                    List.append a
                      [
                        ( third
                            (eval_sample
                               (Distribution.bernoulli ~p:x)
                               env env_alt),
                          p );
                      ])
                  [] a)
              alt
          in

          let alt =
            List.map
              (fun a ->
                List.fold_left
                  (fun a (l, pl) ->
                    List.append a
                      (List.fold_left
                         (fun b (x, p) -> List.append b [ (x, pl *. p) ])
                         [] l))
                  [] a)
              alt
          in
          (pval, env, alt, env_alt)
      | UniformD (e1, e2) ->
          let _, _, alt1, _ = eval_expr env e1 clist env_alt in
          let _, _, alt2, _ = eval_expr env e2 clist env_alt in

          let op (x, px) (y, py) = ((x, y), px *. py) in
          let op' l1 l2 = op_list op l1 l2 in
          let alt = op_list op' alt1 alt2 in
          let alt =
            List.map
              (fun a ->
                List.fold_left
                  (fun a ((x, y), p) ->
                    List.append a
                      [
                        ( third
                            (eval_sample
                               (Distribution.uniform_discrete ~a:x ~b:y)
                               env env_alt),
                          p );
                      ])
                  [] a)
              alt
          in
          let alt =
            List.map
              (fun a ->
                List.fold_left
                  (fun a (l, pl) ->
                    List.append a
                      (List.fold_left
                         (fun b (x, p) -> List.append b [ (x, pl *. p) ])
                         [] l))
                  [] a)
              alt
          in

          (0., env, alt, env_alt))
  | Assume _ -> (0., env, [], env_alt)
  | _ -> failwith "nyi"

let rec print_cond c =
  match c with
  | Or (c1, c2) ->
      print_cond c1;
      Printf.printf " or ";
      print_cond c2;
      Printf.printf " "
  | Eq (v, f) -> Printf.printf "%s = %f" v f
  | Not c ->
      Printf.printf "not ";
      print_cond c
  | And (c1, c2) ->
      print_cond c1;
      Printf.printf " and ";
      print_cond c2;
      Printf.printf " "
  | True -> Printf.printf "true "
  | False -> Printf.printf "false"

let normalise alt =
  let sum = List.fold_left (fun a -> fun (_,p) -> a +. p  ) 0. alt in 
  let rec aux alt n  =
    match alt with [] -> [] | (x, p) :: q -> (x, p /. n) :: aux q n
  in
  aux alt  sum

let infer m =
  match m with
  | Let (_, e) ->
      let clist = find_assume e in
      List.iter
        (fun c ->
          print_cond c;
          print_endline "")
        clist;
      let _, _, alt, _ = eval_expr [] e clist [] in
      let vlist, plist = List.split ( normalise (List.flatten alt)) in

      Distribution.support_p ~values:(Array.of_list vlist)
        ~probs:(Array.of_list plist)

let eval_prog p = match p with Infer m -> infer m
