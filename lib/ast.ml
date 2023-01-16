type var = string

type model = Let of var * expr
and dist = Bernoulli of expr | UniformD of expr * expr

and expr =
  | Const of float
  | Var of var
  | Add of expr * expr
  | Sub of expr * expr
  | IfThenElse of expr * expr * expr
  | LetIn of var * expr * expr
  | Sample of dist
  | Assume of cond
  | Observe of cond

and expr_int =
| ConstA of float
| VarA of var
| AddA of expr * expr
| SubA of expr * expr
| IfThenElseA of expr * expr * expr
| LetInA of var * expr * expr
| SampleA of dist
| AssumeA of cond
| ObserveA of cond


and cond =
  | Or of cond * cond
  | Eq of var * float
  | Not of cond
  | And of cond * cond
  | True
  | False

type prog = Infer of model

(* ("x",P(x=v),v)*)
type env = (var * float) list
type env_alt = (var * (float * float) list) list list
