(* calc.sml *)

(* This file provides glue code for building the calculator using the
 * parser and lexer specified in calc.lex and calc.grm.
*)

structure Repl:
sig
  val parse: string -> unit
end =
struct

  (*
   * We apply the functors generated from Main.lex and Main.grm to produce
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
   * this. It runs the Mainulator on the standard input and terminates when
   * an end-of-file is encountered.
   *)

  fun parse path =
    let

      val f = TextIO.openIn path
      val lexer = MainParser.makeLexer (fn _ =>
        case (TextIO.inputLine f) of
          SOME s => s
        | _ => "")
      val dummyEOF = MainLrVals.Tokens.EOF (0, 0)
      fun loop lexer =
        let
          val (result, lexer) = invoke lexer
          val (nextToken, lexer) = MainParser.Stream.get lexer
          val _ =
                TextIO.output
                  (TextIO.stdOut, "result = " ^ (Ast.pp_declist result) ^ "\n")
        in
          if MainParser.sameToken (nextToken, dummyEOF) then () else loop lexer
        end
    in
      loop lexer
    end

end (* structure Main *)
