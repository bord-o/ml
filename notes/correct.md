VAL RECackermann
 Lam m
         Lam n
                 If:
                         Prim Equal
                                 Var m,
                                 Lit 0
                         Prim Add
                                 Var n,
                                 Lit 1
                         If:
                                 Prim And
                                         Prim Greater than
                                                 Var m,
                                                 Lit 0,
                                         Prim Equal
                                                 Var n,
                                                 Lit 0
                                 App
                                         App
                                                 Var ackermann,
                                                 Prim Sub
                                                         Var m,
                                                         Lit 1,
                                         Lit 1
                                 If:
                                         Prim And
                                                 Prim Greater than
                                                         Var m,
                                                         Lit 0,
                                                 Prim Greater than
                                                         Var n,
                                                         Lit 0
                                         App
                                                 App
                                                         Var ackermann,
                                                         Prim Sub
                                                                 Var m,
                                                                 Lit 1,
                                                 App
                                                         App
                                                                 Var ackermann,
                                                                 Var m,
                                                         Prim Sub
                                                                 Var n,
                                                                 Lit 1
                                         Lit 99
VAL: res
 App
         App
                 Var ackermann,
                 Lit 3,
         Lit 3

