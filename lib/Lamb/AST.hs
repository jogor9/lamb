module Lamb.AST
  ( Pattern,
    Decl (..),
    Def (..),
    Expr (..),
    TopLevel (..),
  )
where

import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty)
import Data.Scientific (Scientific)
import Data.Text (Text)

type Pattern = NonEmpty (Maybe Expr, Expr, Maybe Expr)

data Decl = Decl (Text, Maybe Pattern) [(Pattern, Maybe Pattern)]
  deriving (Read, Show, Eq)

data Def = Def Decl Expr
  deriving (Read, Show, Eq)

data Expr
  = List [Expr]
  | RangeList Expr (Maybe Expr) (Maybe Expr)
  | Dict (HashMap Text Expr)
  | Tuple Expr Expr
  | Unit
  | BoolLiteral Bool
  | Hole
  | Name Text
  | Numeral Scientific
  | StringLiteral Text
  | CharLiteral Char
  | Compound (NonEmpty Expr)
  | Application Expr (NonEmpty Expr)
  | NameAccess Expr Expr
  | IndexAccess Expr Expr
  | Power Expr Expr
  | UMinus Expr
  | Times Expr Expr
  | Divide Expr Expr
  | Modulo Expr Expr
  | Plus Expr Expr
  | Minus Expr Expr
  | Concatenation Expr Expr
  | Equals Expr Expr
  | NotEquals Expr Expr
  | Lt Expr Expr
  | Gt Expr Expr
  | Le Expr Expr
  | Ge Expr Expr
  | MemberOf Expr Expr
  | Conjunction Expr Expr
  | Alternation Expr Expr
  | Compose Expr Expr
  | MonadicCompose Expr Expr
  | RevCompose Expr Expr
  | RevMonadicCompose Expr Expr
  | Parallel Expr Expr
  | IfThenElse Expr Expr Expr
  | Let (NonEmpty Def) Expr
  | DoWhile (NonEmpty Expr) Expr
  | WhileDo Expr Expr
  | Guard (NonEmpty (Expr, Expr)) Expr
  | IfDo Expr Expr
  | Case Expr (NonEmpty (Maybe Expr, Expr, Maybe Expr, Expr))
  | Pipe Expr Expr
  | Bind Expr Expr
  | Over Expr Expr
  | RevPipe Expr Expr
  | RevBind Expr Expr
  | RevOver Expr Expr
  | Alternative Expr Expr
  | Assign Expr Expr
  | PlusAssign Expr Expr
  | MinusAssign Expr Expr
  | TimesAssign Expr Expr
  | DivideAssign Expr Expr
  | ModuloAssign Expr Expr
  | Lambda (NonEmpty (Pattern, Maybe Pattern)) Expr
  deriving (Show, Eq, Read)

data TopLevel
  = TopLevelDecl Decl
  | TopLevelDef Def
  | TopLevelExpr Expr
  deriving (Read, Show, Eq)
