# lamb

My custom functional programming language. Currently only the parser is implemented. Run `cabal run | dot -Tpng | ffplay -f image2pipe -`, and type e.g. `2 + if cond then a else b` to get an AST visualization.
