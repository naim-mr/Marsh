open Ast 
let model  v e = Let (v,e)
let const f = Const f 
let var x = Var x 
let add e1 e2 = Add (e1,e2)

let sub e1 e2 = Sub (e1,e2)


let ifthenelse e1 e2 e3 = IfThenElse (e1,e2,e3)

let letin v e1 e2  = LetIn (v,e1,e2)

let sample dist = Sample dist 

let d_bernoulli p = Bernoulli p 
let d_uniformd a b = UniformD (a,b)
let assume e = Assume e 

let observe e = Observe e 
