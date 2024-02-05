structure Ast =
struct

  type name = string
  datatype primop = Add | Mul | Eq

  datatype expr =
    Var of name
  | Lam of name * expr
  | App of expr * expr
  | Lit of int
  | Prim of primop * expr * expr
  | If of expr * expr * expr

  datatype dec =
    Val of name * expr
    | ValRec of name * expr

  datatype value = VInt of int | VClosure of (value -> value)

  fun pp_op o' =
    case o' of
      Add => "Add"
    | Mul => "Mul"
    | Eq => "Equal"

  fun pp_value (VInt i) = Int.toString i
    | pp_value (VClosure _) = "<<closure>>"

  fun pp_ast a =
    case a of
      Var n => (" Var " ^ n)
    | Lam (n,e) => (" Lam " ^ n ^ (pp_ast e))
    | App (e1, e2) => (" App " ^ (pp_ast e1) ^ ", " ^ (pp_ast e2))
    | Lit i => (" Lit " ^ (Int.toString i))
    | Prim (op', e1, e2) =>
        (" Prim " ^ (pp_op op') ^ " " ^ (pp_ast e1) ^ ", " ^ (pp_ast e2))
    | If (if',then',else') =>
        (" If: " ^ (pp_ast if') ^ " Then: " ^(pp_ast then')^ " Else: " ^ (pp_ast else'))

        
end
