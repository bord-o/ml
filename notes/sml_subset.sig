type name = string

datatype exp
  = VarExp of name
  | FnExp of name
  | AppExp of {function : exp, argument : exp}
  | LetExp of {dec : dec, expr : exp}
  | SeqExp of exp list
  | IntExp of int
  | IfExp of {test : exp, thenCase : exp, elseCase : exp}
  | AndalsoExp of (exp * exp)
  | OrelseExp of (exp * exp)

datatype dec
  = ValDec of name * exp
