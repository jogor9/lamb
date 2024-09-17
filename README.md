# lamb

My custom functional programming language.

Run `cabal run lamb` to get into an interactive session.

Run `cabal run lamb-ast | dot -Tpng | ffplay -f image2pipe -`, and type e.g. `2 + if cond then a else b` to get an AST visualization.
