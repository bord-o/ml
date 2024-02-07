open Util
infix 3 |>
infixr 3 <|

  
(* just the right hand side, the name will be provided by the parser *)
val fact = 
    Ast.Lam("n",
      Ast.If (
        Ast.Prim(
          Ast.Eq, 
          Ast.Var "n", 
          Ast.Lit 0),
        Ast.Lit 1,
        Ast.Prim(
          Ast.Mul,
          Ast.Var "n",
          Ast.App(
            Ast.Var "fact",
            Ast.Prim(
              Ast.Add,
              Ast.Lit ~1,
              Ast.Var "n"
            )
          )
        )
      )
    )

  val ack = 
    Ast.Lam ("m",
      Ast.Lam("n",
        Ast.If(
          Ast.Prim(Ast.Eq, Ast.Var "m", Ast.Lit 0),
          Ast.Prim(Ast.Add, Ast.Var "n", Ast.Lit 1),
          Ast.If(
            Ast.Prim(Ast.And,
              Ast.Prim(Ast.Gt, Ast.Var "m", Ast.Lit 0),
              Ast.Prim(Ast.Eq, Ast.Var "n", Ast.Lit 0)
            ),
            Ast.App(
              Ast.App(Ast.Var "ack",
               (Ast.Prim (Ast.Add, Ast.Var "m", Ast.Lit ~1))
              ),
              Ast.Lit 1
            ),
            Ast.If(
              Ast.Prim(Ast.And,
                Ast.Prim(Ast.Gt, Ast.Var "m", Ast.Lit 0),
                Ast.Prim(Ast.Gt, Ast.Var "n", Ast.Lit 0)
              ),
              Ast.App(
                Ast.App(Ast.Var "ack",
                 (Ast.Prim (Ast.Add, Ast.Var "m", Ast.Lit ~1))
                ),
                Ast.App(
                  Ast.App(Ast.Var "ack",
                    Ast.Var "m"
                  ),
                   (Ast.Prim (Ast.Add, Ast.Var "n", Ast.Lit ~1))
                )
              ),
              Ast.Lit ~99
            )
          )
        )
      )
    )

(*
  When we start parsing, we will have declarations: Ast.Val of string * Ast.expr
  and Ast.ValRec of string * Ast.expr, but ValRec will use the z combinator and 
  setup function to ensure that recursive calls work for the resulting closure
*)    
val _ = print "Starting interpreter...\n\n"

val declist =  [
  Ast.ValRec ("fact", fact),
  Ast.Val ("ans", Ast.App (Ast.Var "fact", Ast.Lit 5)),
  Ast.ValRec ("ack", ack),
  Ast.Val ("ans2", Ast.App(Ast.App(Ast.Var "ack", Ast.Lit 3),  Ast.Lit 2))
]

val env : Interp.env = 
  Dict.empty op=
  |> Dict.insert "true" (Ast.VInt 1) 
  |> Dict.insert "false" (Ast.VInt 0) 

val finalenv = Interp.run env declist 
val p = Dict.pp_dict id Ast.pp_value finalenv
val _ = print p
