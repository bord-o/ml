(* calc.sml *)

(* This file provides glue code for building the calculator using the
 * parser and lexer specified in calc.lex and calc.grm.
*)

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
  exception Todo
  exception NotAClosure
  exception VarUnbound
  type env = value list
  (* val env' : (string * int) HashTable.hash_table = HashTable.mkTable (HashString.hashString, String.compare) (1024, VarUnbound)  *)


  fun evalPrim Add (VInt a) (VInt b) =
        VInt (a + b)
    | evalPrim Mul (VInt a) (VInt b) =
        VInt (a * b)
    | evalPrim _ _ _ = raise Todo

  fun eval (env: env) (term: expr) : value =
    case term of
      Var i => List.nth (env, i)
    | Lit i => VInt i
    | Lam e => VClosure (fn v => eval (v :: env) e)
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

end


val _ = print "Starting interpreter...\n\n"
val program = Ast.Prim (Ast.Mul, Ast.Lit 2, Ast.Lit 2)
val _ = (print o Ast.pp_ast) program
val _ = print "\n\n"
val env = []
val res = Interp.eval env program
val _ = print (Ast.pp_value res)
val _ = print "\n\n"
