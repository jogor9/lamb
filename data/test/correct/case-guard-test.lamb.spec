[TopLevelExpr (Plus (Name "c") (Case (Name "x") ((Nothing,Name "y",Nothing,Name "z") :| [(Just (Name "a"),Name "b",Nothing,Name "c"),(Nothing,Name "d",Just (Name "e"),Name "f"),(Just (Name "g"),Name "h",Just (Name "i"),Name "j")]))),TopLevelExpr (Plus (Numeral 2.0) (Guard ((Name "guard",Numeral 4.0) :| []) (Numeral 5.0))),TopLevelExpr (Plus (UMinus (Power (Numeral 2.0) (Numeral 3.0))) (Let (Def (Decl ("n",Nothing) []) (Numeral 45.0) :| [Def (Decl ("m",Nothing) []) (Numeral 69.0)]) (Times (Name "m") (Name "n"))))]