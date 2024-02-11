structure Interp =
struct
  open Ast
  open Util
  exception Todo
  exception NotAClosure of string
  exception NotAnInt
  exception VarUnbound of string
  type env = (string, value) Dict.dict
  (* val env' : (string * int) HashTable.hash_table = HashTable.mkTable (HashString.hashString, String.compare) (1024, VarUnbound)  *)


  val z =
    Ast.Lam
      ( "f"
      , Ast.App
          ( Ast.Lam ("x", Ast.App (Ast.Var "f", Ast.Lam ("v", Ast.App
              ((Ast.App (Ast.Var "x", Ast.Var "x")), Ast.Var "v"))))
          , Ast.Lam ("x", Ast.App (Ast.Var "f", Ast.Lam ("v", Ast.App
              ((Ast.App (Ast.Var "x", Ast.Var "x")), Ast.Var "v"))))
          )
      )

  fun evalPrim Add (VInt a) (VInt b) =
        VInt (a + b)
    | evalPrim Mul (VInt a) (VInt b) =
        VInt (a * b)
    | evalPrim Sub (VInt a) (VInt b) =
        VInt (a - b)
    | evalPrim Div (VInt a) (VInt b) =
        if b = 0 then VInt 0 else VInt (Int.div (a,b))
    | evalPrim Eq (VInt a) (VInt b) =
        VInt (if a = b then 1 else 0)
    | evalPrim Gt (VInt a) (VInt b) =
        VInt (if a > b then 1 else 0)
    | evalPrim Lt (VInt a) (VInt b) =
        VInt (if a < b then 1 else 0)
    | evalPrim And (VInt a) (VInt b) =
        VInt (if a = 1 andalso b = 1 then 1 else 0)
    | evalPrim Or (VInt a) (VInt b) =
        VInt (if a = 1 orelse b = 1 then 1 else 0)
    | evalPrim _ _ _ = raise Todo

  fun eval (env: env) (term: expr) : value =
    case term of
      Var n =>
        (*(print ("Looking up: " ^ n ^ " in env: \n" ^ (Util.Dict.pp_dict Util.id Ast.pp_value env) ^ "\n\n")
        ;*)
        (Dict.find_exn n env
         handle Util.Dict.NotFound => (print (n ^ "\n"); raise VarUnbound n))

    | Lit i => VInt i
    | Lam (n, e) => VClosure (fn v => eval (Util.Dict.insert n v env) e)
    | App (e1, e2) =>
        let
          val c =
            case eval env e1 of
              VClosure c => c
            | _ => raise (NotAClosure (Ast.pp_ast e1))
          val v = eval env e2
        in
          c v
        end
    | Prim (op', e1, e2) => evalPrim op' (eval env e1) (eval env e2)
    | If (if', then', else') =>
        let
          val eval = eval env
          val cond =
            case eval if' of
              VClosure _ => raise NotAnInt
            | VInt n => n <> 0
        in
          if cond then eval then' else eval else'
        end

  fun run (env: env) (program: dec list) : env =
    let
      (* 
        Running into errors with recursion here because the initial closure we create
        does not have the calling recursive function in it:
      
        env_y = environment with recursive function bound
        env_n = env without recursive function bound
      
        1:
          bind recf with env_n
        2:
          bind ans with env_y (successfully looks up recf)      
        3: 
          execute closure 1 to get value for 2
        exec:
          FAILS on lookup for recf in 1
      *)
      (* This takes a recursive function f that is named name and makes it work with z comb*)
      fun setup (f: Ast.expr) (name: string) =
        Ast.App (z, Ast.Lam (name, f))
      fun eval_dec (Val (n, e)) env' =
            let
              val newenv = Util.Dict.insert n (eval env' e) env'
            in
              ( (*print "\nBEFORE UPDATE\n";
                print (Util.Dict.pp_dict Util.id Ast.pp_value env');
                print "\nAFTER UPDATE\n";
                print (Util.Dict.pp_dict Util.id Ast.pp_value newenv);*)newenv)
            end
        | eval_dec (ValRec (n, e)) env' =
            let
              val newenv = Util.Dict.insert n (eval env' (setup e n)) env'
            in
              ( (* print "\nBEFORE UPDATE\n";
                print (Util.Dict.pp_dict Util.id Ast.pp_value env');
                print "\nAFTER UPDATE\n";
                print (Util.Dict.pp_dict Util.id Ast.pp_value newenv);*)newenv)
            end
    in
      foldl
        (fn (d, e) =>
           ( (*print (Util.Dict.pp_dict Util.id Ast.pp_value e);*)eval_dec d e))
        env program
    end
end
