[
  TopLevelDef (
    Def (Decl
      (
        "quickSort",
        [(Just (List [Name "any"]),
          List [Name "any"],
          Just (
            Application
            (Name "Compare")
            (Name "any" :| [])
          )
        )]
      )
      []
    )
    (
      Compose
      (NameAccess Hole (Name "uncons"))
      (
        Case
        Hole
        (
          (Nothing,List [],Nothing,
            List []
          ) :| [
          (Nothing,List [Name "head",Name "tail"],Nothing,
            Pipe
            (Pipe (Name "tail") (Application (Name "partition") (Lt Hole (Name "head") :| [])))
            (Lambda (
              LambdaDef
              ((Tuple (Name "a") (Name "b"),[]) :| [])
              (
                Concatenation
                (Concatenation (Application (Name "quickSort") (Name "a" :| [])) (List [Name "head"]))
                (Application (Name "quickSort") (Name "b" :| []))
              )
            ))
          )]
        )
      )
    )
  ),
  TopLevelExpr (
    RevPipe
    (Name "print")
    (
      Application
      (Name "quickSort")
      (List [
        Numeral 3.0,
        Numeral 4.0,
        Numeral 9.0,
        Numeral 3.0,
        Numeral 10.0,
        Numeral 15.0,
        Numeral 16.0,
        Numeral 2.0,
        Numeral 1.0
      ] :| [])
    )
  )
]
