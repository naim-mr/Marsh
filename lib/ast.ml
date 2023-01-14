type var = string 




type model = Let of var*expr 
and
dist = Bernoulli of float | UniformD of float*float

and 
expr = Const of float 
       | Var of var 
       | Add of expr*expr
       | Sub of expr*expr
       | IfThenElse of expr*expr*expr
       | LetIn of var*expr*expr
       | Sample of dist 
       | Assume of expr
       | Observe of expr      


type prog = Infer of model

(* ("x",P(x=v),v)*)
type env = (var*float ) list
type env_alt = (var*(float*float) list) list
 



