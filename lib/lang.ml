open Ast 
let model  v e = Let (v,e)
let const f = Const f
let var x = Var x 
let add e1 e2 = Add (e1,e2)

let sub e1 e2 = Sub (e1,e2)


let ifthenelse e1 e2 e3 = IfThenElse (e1,e2,e3)

let letin v e1 e2  = LetIn (v,e1,e2)

let sample dist = Sample dist 

let d_bernoulli e = Bernoulli e 
let d_uniformd a b = UniformD (a,b)
let assume c = Assume c 

let or_c c1 c2 = Or(c1,c2)
let and_c c1 c2 = And(c1,c2)
let eq x f = Eq(x,f)
let not c = Not c
let tr = True
let flse = False
let observe e = Observe e 
