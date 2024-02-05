(* calc.sml *)

(* This file provides glue code for building the calculator using the
 * parser and lexer specified in calc.lex and calc.grm.
*)
infix 3 |>
infixr 3 <|

structure Main:
sig
  val parse: unit -> unit
  val readAllText: string -> string
end =
struct

  (*
   * We apply the functors generated from calc.lex and calc.grm to produce
   * the MainParser structure.
   *)

  structure MainLrVals = MainLrValsFun (structure Token = LrParser.Token)
  structure MainLex = MainLexFun (structure Tokens = MainLrVals.Tokens)

  structure MainParser =
    Join
      (structure LrParser = LrParser
       structure ParserData = MainLrVals.ParserData
       structure Lex = MainLex)

  (*
   * We need a function which given a lexer invokes the parser. The
   * function invoke does this.
   *)

  fun invoke lexstream =
    let
      fun print_error (s, i: int, _) =
        TextIO.output
          (TextIO.stdOut, "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
    in
      MainParser.parse (0, lexstream, print_error, ())
    end

  (*
   * Finally, we need a driver function that reads one or more expressions
   * from the standard input. The function parse, shown below, does
   * this. It runs the calculator on the standard input and terminates when
   * an end-of-file is encountered.
   *)
  fun readAllText (path: string) =
    let
      val infile = TextIO.openIn path
      val text = TextIO.inputAll infile
    in
      (TextIO.closeIn infile; text)
    end

  fun parse () =
    let
      val file = "/home/bordo/ml/test/test1.sml"
      val fstream = TextIO.openIn file
      val lexer = MainParser.makeLexer (fn _ =>
        (case TextIO.inputLine fstream of
           SOME s => s
         | _ => ""))
      val dummyEOF = MainLrVals.Tokens.EOF (0, 0)
      val dummySEMI = MainLrVals.Tokens.SEMI (0, 0)
      fun loop lexer =
        let
          val (result, lexer) = invoke lexer
          val (nextToken, lexer) = MainParser.Stream.get lexer
          val _ =
            case result of
              SOME r =>
                TextIO.output
                  (TextIO.stdOut, "result = " ^ (Int.toString r) ^ "\n")
            | NONE => ()
        in
          (* check if at end of file *)
          if MainParser.sameToken (nextToken, dummyEOF) then () else loop lexer
        end
    in
      loop lexer
    end

end (* structure Main *)

structure Interp =
struct
  open Ast
  open Util
  exception Todo
  exception NotAClosure
  exception NotAnInt
  exception VarUnbound of string
  type env = (string, value) Dict.dict
  (* val env' : (string * int) HashTable.hash_table = HashTable.mkTable (HashString.hashString, String.compare) (1024, VarUnbound)  *)

  
  val z = 
    Ast.Lam ("f",
      Ast.App (
        Ast.Lam("x",
          Ast.App (
            Ast.Var "f",
            Ast.Lam("v",
              Ast.App(
                (Ast.App( Ast.Var "x", Ast.Var "x")),
                Ast.Var "v"
              )
            )
          )
        ),
        Ast.Lam("x",
          Ast.App (
            Ast.Var "f",
            Ast.Lam("v",
              Ast.App(
                (Ast.App ( Ast.Var "x", Ast.Var "x")),
                Ast.Var "v"
              )
            )
          )
        )
      )
    )

  fun evalPrim Add (VInt a) (VInt b) =
        VInt (a + b)
    | evalPrim Mul (VInt a) (VInt b) =
        VInt (a * b)
    | evalPrim Eq (VInt a) (VInt b) =
        VInt (if a = b then 1 else 0)
    | evalPrim _ _ _ = raise Todo

  fun eval (env: env) (term: expr) : value =
    case term of
    Var n => 
      (print ("Looking up: " ^ n ^ " in env: \n" ^ (Util.Dict.pp_dict Util.id Ast.pp_value env) ^ "\n\n")
      ; (Dict.find_exn n env handle Util.Dict.NotFound => (print ( n ^ "\n");raise VarUnbound n))
      )
    | Lit i => VInt i
    | Lam (n,e) => VClosure (fn v => eval (Util.Dict.insert n v env) e)
    | App (e1, e2) =>
        let
          val c =
            case eval env e1 of
              VClosure c => c
            | _ => raise NotAClosure
          val v = eval env e2
        in
          c v
        end
    | Prim (op', e1, e2) => evalPrim op' (eval env e1) (eval env e2)
    | If (if',then',else') => 
      let
        val eval = eval env
        val cond = 
          case eval if' of
          VClosure _ => raise NotAnInt
          | VInt n => n <> 0 
      in
        if cond then eval then' else eval else'
      end

  fun run (env:env) (program : dec list) : env =  
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
      fun setup (f : Ast.expr) (name : string) = 
        Ast.App(
          z,
          Ast.Lam(name,
            f
          )
        )
      fun eval_dec (Val (n, e)) env' = 
        let 
          val newenv = Util.Dict.insert n (eval env' e) env'
        in
        (
          print "\nBEFORE UPDATE\n";
          print (Util.Dict.pp_dict Util.id Ast.pp_value env');
          print "\nAFTER UPDATE\n";
          print (Util.Dict.pp_dict Util.id Ast.pp_value newenv);
          newenv
        )
        end
      | eval_dec (ValRec (n, e)) env' = 
        let 
          val newenv = Util.Dict.insert n (eval env' (setup e n)) env'
        in
        (
          print "\nBEFORE UPDATE\n";
          print (Util.Dict.pp_dict Util.id Ast.pp_value env');
          print "\nAFTER UPDATE\n";
          print (Util.Dict.pp_dict Util.id Ast.pp_value newenv);
          newenv
        )
        end
    in
      foldl (fn (d, e) => 
        ((*print (Util.Dict.pp_dict Util.id Ast.pp_value e);*)  eval_dec d e)) env program
    end
end


val _ = print "Starting interpreter...\n"
(*
val program1 = Ast.Prim (Ast.Mul, Ast.Lit 2, Ast.Lit 2)
val program = Ast.App ((Ast.Lam ("a" , 
                                (Ast.Prim (Ast.Add, 
                                    Ast.Var "a", 
                                    Ast.Lit 11 ) ))) , (Ast.Lit 2))

val program = (Ast.Lam ("a" , 
                        (Ast.Prim (Ast.Add, 
                            Ast.Var "a", 
                            Ast.Lit 11 ) )))

val fact = 
    Ast.Lam("n", Ast.App 
      (Ast.Var "fact", 
        (Ast.Prim (Ast.Add, Ast.Lit 1, Ast.Var "n"))) )
    
val application_test = Ast.App (Ast.Var "sum11" , Ast.Lit 2)
val declist =  [
  Ast.Dec ("sum11", program),
  Ast.Dec ("ans", application_test)

]
*)


  
(* just the right hand side *)
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

(*
  When we start parsing, we will have declarations: Ast.Val of string * Ast.expr
  and Ast.ValRec of string * Ast.expr, but ValRec will use the z combinator and 
  setup function to ensure that recursive calls work for the resulting closure
*)    

val declist =  [
  Ast.ValRec ("fact", fact) ,
  Ast.Val ("ans", Ast.App (Ast.Var "fact", Ast.Lit 5))

]


val _ = (print o Ast.pp_ast) fact
val _ = print "\n"
(* val res = Interp.eval env program *)
(* val _ = print (Ast.pp_value res) *)
(* val _ = print "\n\n" *)
open Util
val env : Interp.env = 
  Dict.empty op=
  |> Dict.insert "true" (Ast.VInt 1) 
  |> Dict.insert "false" (Ast.VInt 0) 

val finalenv = Interp.run env declist 
val p = Util.Dict.pp_dict Util.id Ast.pp_value finalenv
val _ = print p
