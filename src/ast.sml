structure Ast =
struct

  type name = string
  datatype primop = Add | Mul | Div | Sub | Eq | Gt | Lt | And | Or

  datatype expr =
    Var of name
  | Lam of name * expr
  | App of expr * expr
  | Lit of int
  | Prim of primop * expr * expr
  | If of expr * expr * expr
  | Let of dec list * expr

  and dec = Val of name * expr | ValRec of name * expr

  datatype value = VInt of int | VClosure of (value -> value)

  fun pp_op o' =
    case o' of
      Add => "Add"
    | Mul => "Mul"
    | Div => "Div"
    | Sub => "Sub"
    | Eq => "Equal"
    | Gt => "Greater than"
    | Lt => "Less than"
    | And => "And"
    | Or => "Or"

  fun apply (l : expr list): expr = 
    raise (Fail "todo: apply list")

  fun pp_value (VInt i) = Int.toString i
    | pp_value (VClosure _) = "<<closure>>"
 
  fun tabs n =
    foldl (fn (item, state) => item ^ state ) "" (List.tabulate (n, fn _ => "\t"))

  fun pp_ast ast ilevel =
    let
        
      fun aux a i = 
        let
          val indent = "\n" ^ tabs i
        in
          indent ^
          (case a of
            Var n => ("Var " ^ n)
          | Lam (n, e) => ("Lam " ^ n ^ (aux e (i+1)))
          | App (e1, e2) => ("App " ^ (aux e1 (i+1)) ^ ", " ^ (aux e2 (i+1)))
          | Lit i' => ("Lit " ^ (Int.toString i' ))
          | Prim (op', e1, e2) =>
              ("Prim " ^ (pp_op op') ^ " " ^ (aux e1 (i+1)) ^ ", " ^ (aux e2 (i+1)))
          | If (if', then', else') =>
              ("If: " ^ (aux if' (i+1)) ^  (aux then' (i+1)) ^ (aux else' (i+1)))
          | Let (decs, e) =>
              ("Let:" ^ (pp_declist decs (i+1)) ^ (aux e (i+1))))
        end
    in
      aux ast ilevel
    end

  and pp_declist (l: dec list) indent =
    let
      fun printer (d : dec) = 
        case d of
        Val (n, e) => ( "\n" ^ (tabs indent) ^"DEC VAL: " ^ n ^  pp_ast e (indent+1))
        | ValRec (n, e) => ("\n"  ^ (tabs indent) ^ "DEC VAL REC" ^ n  ^ pp_ast e (indent+1))
    in
      foldr (fn (item,state) => (printer item) ^ state) "" l
    end

end
