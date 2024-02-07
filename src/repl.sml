
structure Repl:
sig
  val readAllText: string -> string
  val parse: unit -> unit
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
