structure Ast = struct

datatype primop 
     = Add 
     | Mul

datatype expr 
     = Var of int
     | Lam of expr
     | App of expr * expr     
     | Lit of int
     | Prim of primop * expr * expr

datatype value
     = VInt of int
     | VClosure of (value -> value)

fun pp_op o' =
     case o' of
     Add => "Add"
     | Mul => "Mul"

fun pp_value (VInt i) = Int.toString i
  | pp_value (VClosure _) = "<<closure>>"

fun pp_ast a = 
     case a of
     Var i =>  ("Var " ^ (Int.toString i))
     | Lam e => ("Lam " ^ (pp_ast e))
     | App (e1, e2) => ("App " ^ (pp_ast e1) ^ ", " ^ (pp_ast e2)) 
     | Lit i =>  ("Lit " ^ (Int.toString i))
     | Prim (op', e1, e2) =>  ("Prim " ^ (pp_op op') ^ " " ^ (pp_ast e1) ^ ", " ^ (pp_ast e2))
end

