open Util
infix 3 |>
infixr 3 <|


(* val _ = InterpTests.testall ()*)

val _ = print "Starting...\n"
val start_env= (Dict.empty op=)
val _ = Dict.pp_dict id Ast.pp_value start_env |> print

val ast = 
    Repl.parse "/home/bordo/ml/test/test.calc"
    handle
    Interp.NotAClosure s => (print ("\n\n" ^ s ^ "\n\n"); raise Interp.NotAClosure s)




val _ = ast |> Ast.pp_declist |> (fn s => s ^ "\n\n") |>  print


val final_env = Interp.run start_env ast
val _ = Dict.pp_dict id Ast.pp_value final_env |> print

