{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Lamb.Parser
  ( parseLamb,
    program,
    expr,
    term,
    appTerm,
    caseTerm,
    condTerm,
    letTerm,
    stringLiteral,
    rawStringLiteral,
    charLiteral,
    numeral,
    list,
    parensTerm,
    dict,
    compound,
    def,
    decl,
    lambdaTerm,
    guardTerm,
    whileDo,
    doWhile,
    argDecl,
    name,
    hole,
    escapeSequence,
    Parser,
  )
where

import Control.Arrow ((>>>))
import Control.Monad
import Control.Monad.Combinators.Expr
import qualified Control.Monad.Combinators.NonEmpty as P
import Data.Bifunctor
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Function
import Data.Functor
import qualified Data.HashMap.Strict as Map
import Data.HashSet (HashSet)
import qualified Data.HashSet as HSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.Ratio
import Data.Scientific (scientific)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Lamb.AST
import Numeric
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as P hiding (endBy1, sepBy1, sepEndBy1, some, someTill)
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Printf

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    (P.space1 P.<|> void (P.string "\\\n"))
    (L.skipLineComment "#")
    P.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

operatorChars :: HashSet Char
operatorChars = HSet.fromList "!@%^&*-:<>.?/|+="

operatorChar :: Parser Char
operatorChar =
  lexeme . P.label "operator char" $
    P.satisfy (`HSet.member` operatorChars)

operator :: Text -> Parser Text
operator op =
  lexeme . P.try . P.label "operator" $
    P.string op <* P.notFollowedBy operatorChar

escapeSequence :: Char -> Parser Char
escapeSequence quote =
  P.char '\\'
    >> P.choice
      [ '\x07' <$ P.char 'a',
        '\x08' <$ P.char 'b',
        '\x1b' <$ P.char 'e',
        '\x0c' <$ P.char 'f',
        '\x0a' <$ P.char 'n',
        '\x0d' <$ P.char 'r',
        '\x09' <$ P.char 't',
        '\x0b' <$ P.char 'v',
        '\\' <$ P.char '\\',
        P.char quote,
        P.char 'x'
          >> P.takeP (Just "ascii hex code") 2
          >>= \case
            (T.unpack >>> readHex -> [(num, "")]) -> return $ chr num
            hexCode -> fail $ printf "invalid ascii hex code: '%s'" hexCode,
        P.char 'u'
          >> P.takeP (Just "unicode code point") 4
          >>= \case
            (T.unpack >>> readHex -> [(num, "")]) -> return $ chr num
            codePoint ->
              fail $ printf "invalid unicode code point: '%s'" codePoint,
        P.char 'U'
          >> P.takeP (Just "unicode code point") 6
          >>= \case
            (T.unpack >>> readHex -> [(num, "")])
              | num <= 0x10ffff -> return $ chr num
            codePoint ->
              fail $ printf "invalid unicode code point: '%s'" codePoint
      ]
    P.<?> "escape sequence"

charLiteral :: Parser Expr
charLiteral =
  P.label "character literal" . lexeme $
    P.between (P.char '\'') (P.char '\'') $
      fmap CharLiteral $
        P.satisfy (`notElem` ['\\', '\'']) P.<|> escapeSequence '\''

stringLiteralText :: Parser Text
stringLiteralText =
  P.label "string literal" . lexeme $
    P.between (P.char '"') (P.char '"') $
      fold
        <$> P.many
          ( P.takeWhile1P
              (Just "character or escape sequence")
              (`notElem` ['"', '\\'])
              P.<|> (T.singleton <$> escapeSequence '"')
          )

stringLiteral :: Parser Expr
stringLiteral = StringLiteral <$> stringLiteralText

rawStringLiteralText :: Parser Text
rawStringLiteralText =
  P.label "string literal" . lexeme $
    P.between (P.string "\"\"\"") (P.string "\"\"\"") $
      fold
        <$> P.many
          ( P.takeWhile1P Nothing (/= '"')
              P.<|> P.tokens (/=) "\"\""
              P.<|> P.tokens (/=) "\"\"\""
          )

rawStringLiteral :: Parser Expr
rawStringLiteral = StringLiteral <$> rawStringLiteralText

unsafeReadHex :: (Eq a, Num a) => String -> a
unsafeReadHex =
  readHex
    >>> filter (snd >>> null)
    >>> map fst
    >>> \case
      [] -> error "expected hexadecimal parse"
      x : _ -> x

numeral :: Parser Expr
numeral =
  ( P.char '0'
      *> P.choice
        [ P.char '.'
            >> ( \frac expo ->
                   scientific (read frac) (expo - length frac)
               )
              . toList
              <$> P.some P.digitChar
              <*> P.option 0 (P.char' 'e' *> L.signed (return ()) L.decimal),
          P.char' 'x'
            >> ( \case
                   0 -> \_ _ -> 0
                   int -> \frac expo ->
                     fromRational $
                       (int `shiftL` (4 * length frac) .|. unsafeReadHex frac)
                         `shiftL` abs expo
                         % 1 `shiftL` (4 * length frac + max 0 (-expo))
               )
              <$> L.hexadecimal
              <*> P.option "0" (P.char '.' *> P.some P.hexDigitChar <&> toList)
              <*> P.option 0 (P.char' 'p' *> L.signed (return ()) L.decimal),
          P.char' 'o' >> L.octal <&> fromInteger,
          P.char' 'b' >> L.binary <&> fromInteger,
          P.option 0 L.scientific
        ]
  )
    P.<|> L.scientific
    <&> Numeral
    & lexeme
    P.<?> "number"

semi, comma, arrow, bar, doubleDot, colon, equals, act, while, given :: Parser ()
semi = void $ symbol ";"
comma = void $ symbol ","
arrow = void $ operator "->"
bar = void $ operator "|"
doubleDot = void $ operator ".."
colon = void $ operator ":"
equals = void $ operator "="
act = void $ symbol "do"
while = void $ symbol "while"
given = void $ symbol "if"

-- (a b c -> a * b + c)

parens :: Parser a -> Parser a
parens = P.between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = P.between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = P.between (symbol "{") (symbol "}")

list :: Parser Expr
list =
  P.label "list" . brackets $
    P.choice
      [ (&)
          <$> expr
          <*> P.choice
            [ doubleDot *> P.optional expr
                <&> \rEnd rbegin -> RangeList rbegin Nothing rEnd,
              comma
                *> P.choice
                  [ ( \step -> \case
                        Left rEnd ->
                          \rbegin -> RangeList rbegin (Just step) rEnd
                        Right l ->
                          \rbegin -> List $ rbegin : step : l
                    )
                      <$> expr
                      <*> P.choice
                        [ doubleDot *> P.optional expr <&> Left,
                          (comma *> expr `P.sepEndBy` comma <&> Right)
                            P.<|> return (Right [])
                        ],
                    return $ \rbegin -> List [rbegin]
                  ],
              return $ \rbegin -> List [rbegin]
            ],
        return $ List []
      ]

dict :: Parser Expr
dict =
  P.label "dictionary" . braces $
    Dict . Map.fromList
      <$> ( (,)
              <$> ( nameText
                      P.<|> stringLiteralText
                      P.<|> rawStringLiteralText
                  )
              <* colon
              <*> expr
          )
        `P.sepEndBy` comma

anyOperator :: Parser (Expr -> Expr -> Expr)
anyOperator =
  P.label "operator" . P.choice $
    map
      (\(nm, f) -> f <$ operator nm)
      [ (".", NameAccess),
        ("==", Equals),
        ("!=", NotEquals),
        (":=", Assign),
        -- assignments conflict with their operators
        ("-", Minus),
        ("-=", MinusAssign),
        ("*=", TimesAssign),
        ("*", Times),
        ("/=", DivideAssign),
        ("/", Divide),
        ("%=", ModuloAssign),
        ("%", Modulo),
        -- monadic compose conflicts with power
        ("^>", MonadicCompose),
        ("^", Power),
        -- over conflicts with index access
        ("@>", Over),
        ("@", IndexAccess),
        -- concatenation and plus assign conflicts with plus
        ("++", Concatenation),
        ("+=", PlusAssign),
        ("+", Plus),
        -- lt conflicts with those
        -- parallel conflicts with reverse compose
        ("<&>", Parallel),
        ("<&", RevCompose),
        -- alternative conflicts with reverse pipe
        ("<|>", Alternative),
        ("<|", RevPipe),
        -- these conflict just with lt
        ("<=", Le),
        ("<^", RevMonadicCompose),
        ("<?", RevBind),
        ("<@", RevOver),
        ("<", Lt),
        -- ge conflicts with gt
        (">=", Ge),
        (">", Gt),
        -- conjunction conflicts with compose
        ("&&", Conjunction),
        ("&>", Compose),
        -- alternation conflicts with pipe
        ("||", Alternation),
        ("|>", Pipe),
        -- bind conflicts with member off
        ("?>", Bind),
        ("?", MemberOf)
      ]

-- TODO: postfix sections (maybe write your own expression parser)

parensTerm :: Parser Expr
parensTerm =
  parens $
    P.label "lambda" (P.try (Lambda <$> lamArgs <* arrow <*> expr))
      P.<|> (P.try (anyOperator <* P.lookAhead (P.char ')')) <&> \op -> Hole `op` Hole)
      P.<|> ( (,)
                <$> exprPostfixSections
                <*> ( (comma *> exprPostfixSections `P.sepBy` comma)
                        P.<|> return []
                    )
                >>= \(h, t) -> case t of
                  [] -> return h P.<?> "expression"
                  l -> return (foldr1 Tuple $ h :| l) P.<?> "tuple"
            )
      P.<|> P.label "unit" (return Unit)

keywords :: HashSet Text
keywords =
  HSet.fromList
    [ "otherwise",
      "if",
      "then",
      "else",
      "do",
      "while",
      "true",
      "false",
      "let",
      "case"
    ]

nameText :: Parser Text
nameText =
  P.label "name" . lexeme . P.try $
    (<>)
      <$> P.takeWhile1P
        (Just "identifier")
        isAlpha
      <*> P.takeWhileP
        (Just "identifier")
        (\x -> isAlphaNum x || x == '_')
      >>= \nm ->
        if nm `HSet.member` keywords
          then P.empty
          else return nm

name :: Parser Expr
name = Name <$> nameText

hole :: Parser Expr
hole =
  P.label "hole" . lexeme $
    Hole
      <$ P.char '_'
      <* P.takeWhileP Nothing (\x -> isAlphaNum x || x == '_')

boolLiteral :: Parser Expr
boolLiteral = fmap BoolLiteral $ True <$ symbol "true" P.<|> False <$ symbol "false"

primaryTerm :: Parser Expr
primaryTerm =
  P.choice
    [ boolLiteral,
      name,
      hole,
      numeral,
      list,
      P.try dict,
      compound,
      rawStringLiteral,
      stringLiteral,
      charLiteral,
      parensTerm
    ]

appTerm :: Parser Expr
appTerm =
  ( \f -> \case
      Just xs -> Application f xs
      Nothing -> f
  )
    <$> primaryTerm
    <*> ( (Just <$> P.some primaryTerm P.<?> "application")
            P.<|> return Nothing
        )

caseTerm :: Parser Expr
caseTerm =
  let pat =
        ( \firstExpr (secondExpr, cond, e) ->
            case secondExpr of
              Just p -> (Just firstExpr, p, cond, e)
              Nothing -> (Nothing, firstExpr, cond, e)
        )
          <$> expr
          <*> ( ( \firstExpr cond -> \case
                    Just secondExpr -> (Just firstExpr, cond, secondExpr)
                    Nothing -> (Nothing, cond, firstExpr)
                )
                  <$> (arrow *> expr)
                  <*> P.optional (bar *> expr)
                  <*> P.optional (arrow *> expr)
                    P.<|> ((Nothing,,) . Just <$> (bar *> expr <* arrow) <*> expr)
              )
          P.<?> "case pattern"
   in Case
        <$> (symbol "case" *> expr <* arrow)
        <*> ((:|) <$> pat <*> P.many (P.try $ semi *> pat))
        P.<?> "case expression"

letTerm :: Parser Expr
letTerm =
  Let
    <$> (symbol "let" *> P.try def `P.endBy1` semi)
    <*> expr
    P.<?> "let expression"

guardTerm :: Parser Expr
guardTerm =
  P.label "guard expression" . (bar *>) $
    Guard
      <$> ( (,)
              <$> expr
              <* arrow
              <*> expr
          )
        `P.endBy1` bar
      <* symbol "otherwise"
      <* arrow
      <*> expr

lambdaTerm :: Parser Expr
lambdaTerm =
  Lambda
    <$> (symbol "\\" *> lamArgs <* arrow)
    <*> expr
    P.<?> "lambda expression"

condTerm :: Parser Expr
condTerm =
  ( \c (t, mf) ->
      case mf of
        Just f -> IfThenElse c t f
        Nothing -> IfDo c t
  )
    <$> (given *> exprNoDoWhile)
    <*> ( (,)
            <$> (symbol "then" *> expr)
            <*> (symbol "else" *> expr <&> Just)
              P.<|> (act *> expr <&> (,Nothing))
        )
    P.<?> "conditional expression"

-- needs to parse the expression after 'while' to determine
-- validity
-- do
--    ...;
--    while ... do
--        ...;
--    while ... do
--        ...;
-- while ...;
doWhile :: Parser Expr
doWhile =
  DoWhile <$> (act *> exprTryWhile `P.sepEndBy1` semi) <* while <*> expr
    P.<?> "do-while expression"

termNoWhile :: Parser Expr
termNoWhile = termNoLoop P.<|> doWhile P.<?> "terminal expression"

termTryWhile :: Parser Expr
termTryWhile = termNoWhile P.<|> P.try whileDo P.<?> "terminal expression"

whileDo :: Parser Expr
whileDo =
  WhileDo <$> (while *> exprNoDoWhile) <* act <*> expr
    P.<?> "while-do expression"

termNoLoop :: Parser Expr
termNoLoop =
  P.choice
    [ condTerm,
      caseTerm,
      lambdaTerm,
      letTerm,
      lambdaTerm,
      guardTerm,
      appTerm
    ]
    P.<?> "terminal expression"

termNoDoWhile :: Parser Expr
termNoDoWhile = termNoLoop P.<|> whileDo P.<?> "terminal expression"

term :: Parser Expr
term = termNoDoWhile P.<|> doWhile P.<?> "terminal expression"

compound :: Parser Expr
compound =
  P.label "compound expression" . braces $
    (Compound .) . (:|)
      <$> expr
      <*> P.option [] (semi *> expr `P.sepEndBy` semi)

leftOp,
  rightOp,
  nonAssoc ::
    Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
leftOp op cons = InfixL (cons <$ operator op)
rightOp op cons = InfixR (cons <$ operator op)
nonAssoc op cons = InfixN (cons <$ operator op)

prefix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix op cons = Prefix (cons <$ operator op)

postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
postfix op cons = Postfix (cons <$ operator op)

pattern :: Parser Pattern
pattern =
  ( ( \firstExpr -> \case
        Just secondExpr -> (Just firstExpr,secondExpr,)
        Nothing -> (Nothing,firstExpr,)
    )
      <$> expr
      <*> P.optional (arrow *> expr)
      <*> P.optional (bar *> expr)
  )
    `P.sepBy1` comma
    P.<?> "pattern match"

typeSpec :: Parser Pattern
typeSpec = colon *> patternTerm P.<?> "type pattern"

patternTerm :: Parser Pattern
patternTerm =
  parens pattern
    P.<|> ((\t -> (Nothing, t, Nothing) :| []) <$> primaryTerm)
    P.<?> "pattern"

argDecl :: Parser (Pattern, Maybe Pattern)
argDecl = (,) <$> patternTerm <*> P.optional typeSpec

lamArgs :: Parser (NonEmpty (Pattern, Maybe Pattern))
lamArgs = P.some argDecl

decl :: Parser Decl
decl =
  Decl
    <$> ((,) <$> nameText <*> P.optional typeSpec)
    <*> P.many argDecl
    P.<?> "declaration"

def :: Parser Def
def =
  Def
    <$> decl
    <* equals
    <*> expr
    P.<?> "definition"

prefixSection :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
prefixSection op cons = prefix op (Hole `cons`)

postfixSection :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
postfixSection op cons = postfix op (`cons` Hole)

-- data Assoc
--   = AssocLeft
--   | AssocRight
--   | AssocNone
--   | AssocPref
--   | AssocPost
--
-- data Op = Op
--   { opName :: Text,
--     opAssoc :: Assoc,
--     opPrec :: Int
--   }
--
-- operators :: [Op]
-- operators =
--   [ Op {opName = ".", opAssoc = AssocLeft, opPrec = 15},
--     Op {opName = "@", opAssoc = AssocLeft, opPrec = 15},
--     Op {opName = "^", opAssoc = AssocRight, opPrec = 14},
--     Op {opName = "-", opAssoc = AssocPref, opPrec = 13},
--     Op {opName = "*", opAssoc = AssocLeft, opPrec = 12},
--     Op {opName = "/", opAssoc = AssocLeft, opPrec = 12},
--     Op {opName = "%", opAssoc = AssocLeft, opPrec = 12},
--     Op {opName = "+", opAssoc = AssocLeft, opPrec = 11},
--     Op {opName = "-", opAssoc = AssocLeft, opPrec = 11},
--     Op {opName = "++", opAssoc = AssocLeft, opPrec = 10},
--     Op {opName = "?", opAssoc = AssocNone, opPrec = 9},
--     Op {opName = "==", opAssoc = AssocLeft, opPrec = 8},
--     Op {opName = "!=", opAssoc = AssocLeft, opPrec = 8},
--     Op {opName = "<", opAssoc = AssocLeft, opPrec = 8},
--     Op {opName = ">", opAssoc = AssocLeft, opPrec = 8},
--     Op {opName = "<=", opAssoc = AssocLeft, opPrec = 8},
--     Op {opName = ">=", opAssoc = AssocLeft, opPrec = 8},
--     Op {opName = "&&", opAssoc = AssocLeft, opPrec = 7},
--     Op {opName = "||", opAssoc = AssocLeft, opPrec = 7},
--     Op {opName = "&>", opAssoc = AssocLeft, opPrec = 6},
--     Op {opName = "^>", opAssoc = AssocLeft, opPrec = 6},
--     Op {opName = "<&", opAssoc = AssocRight, opPrec = 5},
--     Op {opName = "<^", opAssoc = AssocRight, opPrec = 5},
--     Op {opName = "<&>", opAssoc = AssocLeft, opPrec = 4},
--     Op {opName = "|>", opAssoc = AssocLeft, opPrec = 3},
--     Op {opName = "@>", opAssoc = AssocLeft, opPrec = 3},
--     Op {opName = "?>", opAssoc = AssocLeft, opPrec = 3},
--     Op {opName = "<|", opAssoc = AssocRight, opPrec = 2},
--     Op {opName = "<@", opAssoc = AssocRight, opPrec = 2},
--     Op {opName = "<?", opAssoc = AssocRight, opPrec = 2},
--     Op {opName = "<|>", opAssoc = AssocLeft, opPrec = 1},
--     Op {opName = ":=", opAssoc = AssocRight, opPrec = 0},
--     Op {opName = "+=", opAssoc = AssocRight, opPrec = 0},
--     Op {opName = "-=", opAssoc = AssocRight, opPrec = 0},
--     Op {opName = "*=", opAssoc = AssocRight, opPrec = 0},
--     Op {opName = "/=", opAssoc = AssocRight, opPrec = 0},
--     Op {opName = "%=", opAssoc = AssocRight, opPrec = 0}
--   ]
--
-- access :: Parser Expr
-- access = foldl' (&) <$> primaryTerm <*> P.many ((operator "." $> flip NameAccess P.<|> operator "@" $> flip IndexAccess) <*> primaryTerm)

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ leftOp "." NameAccess,
      leftOp "@" IndexAccess
    ],
    [rightOp "^" Power],
    [prefix "-" UMinus],
    [ leftOp "*" Times,
      leftOp "/" Divide,
      leftOp "%" Modulo
    ],
    [ leftOp "+" Plus,
      leftOp "-" Minus
    ],
    [leftOp "++" Concatenation],
    [ leftOp "==" Equals,
      leftOp "!=" NotEquals,
      leftOp "<" Lt,
      leftOp ">" Gt,
      leftOp "<=" Le,
      leftOp ">=" Ge
    ],
    [nonAssoc "?" MemberOf],
    [leftOp "&&" Conjunction],
    [leftOp "||" Alternation],
    [ prefixSection "." NameAccess,
      prefixSection "@" IndexAccess,
      prefixSection "^" Power,
      prefixSection "*" Times,
      prefixSection "/" Divide,
      prefixSection "%" Modulo,
      prefixSection "+" Plus,
      prefixSection "++" Concatenation,
      prefixSection "==" Equals,
      prefixSection "!=" NotEquals,
      prefixSection "<" Lt,
      prefixSection ">" Gt,
      prefixSection "<=" Le,
      prefixSection ">=" Ge,
      prefixSection "?" MemberOf,
      prefixSection "&&" Conjunction,
      prefixSection "||" Alternation
    ],
    [ leftOp "&>" Compose,
      leftOp "^>" MonadicCompose
    ],
    [ rightOp "<&" RevCompose,
      rightOp "<^" RevMonadicCompose
    ],
    [leftOp "<&>" Parallel],
    [ leftOp "|>" Pipe,
      leftOp "?>" Bind,
      leftOp "@>" Over
    ],
    [ rightOp "<|" RevPipe,
      rightOp "<?" RevBind,
      rightOp "<@" RevOver
    ],
    [leftOp "<|>" Alternation],
    [ rightOp ":=" Assign,
      rightOp "+=" PlusAssign,
      rightOp "-=" MinusAssign,
      rightOp "*=" TimesAssign,
      rightOp "/=" DivideAssign,
      rightOp "%=" ModuloAssign
    ]
  ]

exprPostfixSections :: Parser Expr
exprPostfixSections =
  makeExprParser
    term
    $ operatorTable
      ++ [ [ postfixSection "." NameAccess,
             postfixSection "@" IndexAccess,
             postfixSection "^" Power,
             postfixSection "*" Times,
             postfixSection "/" Divide,
             postfixSection "%" Modulo,
             postfixSection "+" Plus,
             postfixSection "++" Concatenation,
             postfixSection "==" Equals,
             postfixSection "!=" NotEquals,
             postfixSection "<" Lt,
             postfixSection ">" Gt,
             postfixSection "<=" Le,
             postfixSection ">=" Ge,
             postfixSection "?" MemberOf,
             postfixSection "&&" Conjunction,
             postfixSection "||" Alternation
           ]
         ]

exprNoDoWhile :: Parser Expr
exprNoDoWhile = makeExprParser termNoDoWhile operatorTable

exprTryWhile :: Parser Expr
exprTryWhile = makeExprParser termTryWhile operatorTable

expr :: Parser Expr
expr =
  makeExprParser
    term
    operatorTable

program :: Parser [TopLevel]
program =
  spaceConsumer
    *> P.choice
      [ P.try def <&> TopLevelDef,
        expr <&> TopLevelExpr
      ]
      `P.sepEndBy` semi
    <* P.eof

parseLamb :: String -> Text -> Either String [TopLevel]
parseLamb filename = P.parse program filename >>> first P.errorBundlePretty
