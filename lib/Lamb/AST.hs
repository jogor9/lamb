module Lamb.AST
  ( Pattern,
    Decl (..),
    Def (..),
    LambdaDef (..),
    Expr (..),
    TopLevel (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Scientific (Scientific)
import Data.Text (Text)

type Pattern = [(Maybe Expr, Expr, Maybe Expr)]

data Decl = Decl (Text, Pattern) [(Expr, Pattern)]
  deriving (Read, Show, Eq)

data Def = Def Decl Expr
  deriving (Read, Show, Eq)

data LambdaDef = LambdaDef (NonEmpty (Expr, Pattern)) Expr
  deriving (Read, Show, Eq)

data Expr
  = List [Expr]
  | RangeList Expr (Maybe Expr) (Maybe Expr)
  | Dict [(Expr, Expr)]
  | Tuple Expr Expr
  | Unit
  | Hole
  | Name Text
  | Numeral Scientific
  | StringLiteral Text
  | CharLiteral Char
  | Compound (NonEmpty Expr)
  | Application Expr (NonEmpty Expr)
  | -- prec 16
    NameAccess Expr Expr
  | IndexAccess Expr Expr
  | -- prec 15
    Power Expr Expr
  | -- prec 14
    UMinus Expr
  | -- prec 13
    Times Expr Expr
  | Divide Expr Expr
  | Modulo Expr Expr
  | -- prec 12
    Plus Expr Expr
  | Minus Expr Expr
  | -- prec 11
    Concatenation Expr Expr
  | -- prec 10
    Equals Expr Expr
  | NotEquals Expr Expr
  | Lt Expr Expr
  | Gt Expr Expr
  | Le Expr Expr
  | Ge Expr Expr
  | -- prec 0
    MemberOf Expr Expr
  | -- prec 8
    Conjunction Expr Expr
  | -- prec 7

    Alternation Expr Expr
  | -- prec 6
    Compose Expr Expr
  | MonadicCompose Expr Expr
  | -- prec 5
    RevCompose Expr Expr
  | RevMonadicCompose Expr Expr
  | -- prec 4
    Parallel Expr Expr
  | -- prec 4
    IfThenElse Expr Expr Expr
  | Let (NonEmpty Def) Expr
  | DoWhile (NonEmpty Expr) Expr
  | WhileDo Expr Expr
  | Guard (NonEmpty (Expr, Expr)) Expr
  | IfDo Expr Expr
  | Case Expr (NonEmpty (Maybe Expr, Expr, Maybe Expr, Expr))
  | -- prec 3
    Pipe Expr Expr
  | Bind Expr Expr
  | Over Expr Expr
  | -- prec 2
    RevPipe Expr Expr
  | RevBind Expr Expr
  | RevOver Expr Expr
  | -- prec 1
    Alternative Expr Expr
  | -- prec 0
    Assign Expr Expr
  | PlusAssign Expr Expr
  | MinusAssign Expr Expr
  | TimesAssign Expr Expr
  | DivideAssign Expr Expr
  | ModuloAssign Expr Expr
  | -- prec -1
    Lambda LambdaDef
  deriving (Show, Eq, Read)

data TopLevel
  = TopLevelDecl Decl
  | TopLevelDef Def
  | TopLevelExpr Expr
  deriving (Read, Show, Eq)
