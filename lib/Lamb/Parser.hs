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
import Data.HashSet (HashSet)
import qualified Data.HashSet as HSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.Ratio
import Data.Scientific (fromRationalRepetendUnlimited, scientific)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Debug.Trace
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
symbol = P.try . L.symbol spaceConsumer

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

stringLiteral :: Parser Expr
stringLiteral =
  P.label "string literal" . lexeme $
    P.between (P.char '"') (P.char '"') $
      StringLiteral . fold
        <$> P.many
          ( P.takeWhile1P
              (Just "character or escape sequence")
              (`notElem` ['"', '\\'])
              P.<|> (T.singleton <$> escapeSequence '"')
          )

rawStringLiteral :: Parser Expr
rawStringLiteral =
  P.label "string literal" . lexeme $
    P.between (P.string "\"\"\"") (P.string "\"\"\"") $
      StringLiteral . fold
        <$> P.many
          ( P.takeWhile1P Nothing (/= '"')
              P.<|> P.tokens (/=) "\"\""
              P.<|> P.tokens (/=) "\"\"\""
          )

ilogBase :: (Integral a) => a -> a -> Int
ilogBase b n
  | n == 0 = -1
  | otherwise = 1 + ilogBase b (n `div` b)

unsafeReadHex :: (Eq a, Num a) => String -> a
unsafeReadHex =
  readHex
    >>> filter (snd >>> null)
    >>> map fst
    >>> \case
      [] -> error "expected hexadecimal parse"
      x : _ -> x

charToDigit :: Int -> Char
charToDigit = chr . (ord '0' +)

hexToDecStr :: String -> String
hexToDecStr =
  uncurry (\c -> if c /= 0 then (charToDigit c :) else id)
    . foldr
      ( \d (c, r) ->
          let (nc, nd) = (c + digitToInt d) `divMod` 10
           in (nc, charToDigit nd : r)
      )
      (0, "")

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
                     fst . fromRationalRepetendUnlimited $
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

semi, comma, arrow, bar, doubleDot, colon, equals, act, while :: Parser ()
semi = void $ symbol ";"
comma = void $ symbol ","
arrow = void $ operator "->"
bar = void $ operator "|"
doubleDot = void $ operator ".."
colon = void $ operator ":"
equals = void $ operator "="
act = void $ symbol "do"
while = void $ symbol "while"

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
    P.try
      ( RangeList
          <$> expr
          <*> P.optional (comma *> expr)
          <*> (doubleDot *> P.optional expr)
      )
      P.<|> (List <$> expr `P.sepEndBy` comma)

dict :: Parser Expr
dict =
  P.label "dictionary" . braces $
    Dict
      <$> ( (,)
              <$> ( name
                      P.<|> stringLiteral
                      P.<|> rawStringLiteral
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
        ("-=", MinusAssign),
        ("-", Minus),
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
    P.label "lambda" (P.try (Lambda <$> (LambdaDef <$> lamArgs <* arrow <*> expr)))
      P.<|> (P.try (anyOperator <* P.lookAhead (P.char ')')) <&> \op -> Hole `op` Hole)
      P.<|> ( (,)
                <$> expr
                <*> ( Right
                        <$> anyOperator
                        <* P.lookAhead (P.char ')')
                          P.<|> (Left <$> (comma *> expr `P.sepBy` comma))
                          P.<|> return (Left [])
                    )
                >>= \(h, t) -> case t of
                  Left [] -> return h P.<?> "expression"
                  Left l -> return (foldr1 Tuple $ h :| l) P.<?> "tuple"
                  Right op -> return (h `op` Hole) P.<?> "section"
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

primaryTerm :: Parser Expr
primaryTerm =
  P.choice
    [ name,
      hole,
      numeral,
      list,
      P.try dict,
      compound,
      rawStringLiteral,
      stringLiteral,
      charLiteral
    ]

singleTerm :: Parser Expr
singleTerm = primaryTerm P.<|> parensTerm

appTerm :: Parser Expr
appTerm =
  ( \f -> \case
      Just xs -> Application f xs
      Nothing -> f
  )
    <$> singleTerm
    <*> ( (Just <$> P.some singleTerm P.<?> "application")
            P.<|> return Nothing
        )

caseTerm :: Parser Expr
caseTerm =
  let pat =
        ( \firstExpr (secondExpr, cond, e) ->
            case secondExpr of
              Just p -> (Pattern (Just firstExpr) p cond, e)
              Nothing -> (Pattern Nothing firstExpr cond, e)
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
        <*> P.try pat
          `P.sepBy1` semi
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

-- P.<|> (SingleGuard <$> expr <* arrow <*> expr)

lambdaTerm :: Parser Expr
lambdaTerm =
  (Lambda .)
    <$> (symbol "\\" *> (LambdaDef <$> lamArgs <* arrow))
    <*> expr
    P.<?> "lambda expression"

condTerm :: Parser Expr
condTerm =
  IfThenElse
    <$> (symbol "if" *> expr)
    <*> (symbol "then" *> expr)
    <*> (symbol "else" *> expr)
    P.<?> "conditional expression"

term :: Parser Expr
term =
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

pattern :: Parser Pattern
pattern =
  ( \firstExpr -> \case
      Just secondExpr -> Pattern (Just firstExpr) secondExpr
      Nothing -> Pattern Nothing firstExpr
  )
    <$> expr
    <*> P.optional (arrow *> expr)
    <*> P.optional (bar *> expr)
    P.<?> "pattern match"

declItem :: Parser a -> Parser (a, Maybe Pattern)
declItem nm =
  (,)
    <$> nm
    <*> P.optional
      ( colon
          *> ( parens pattern
                 P.<|> ((\t -> Pattern Nothing t Nothing) <$> primaryTerm)
             )
      )
    P.<?> "name declaration"

argDecl :: Parser (Maybe Text, Maybe Pattern)
argDecl =
  P.label "argument declaration" . declItem $
    Just <$> nameText P.<|> Nothing <$ hole

lamArgs :: Parser (NonEmpty (Maybe Text, Maybe Pattern))
lamArgs = P.some argDecl

decl :: Parser Decl
decl =
  Decl
    <$> declItem nameText
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

expr :: Parser Expr
expr =
  makeExprParser
    term
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
      ],
      map
        Prefix
        [ P.try $
            DoWhile
              <$> ( act
                      *> ( (:|)
                             <$> expr
                             <*> P.option [] (expr `P.sepEndBy` semi)
                         )
                  )
              <* while,
          -- needs to parse the expression after 'while' to determine
          -- validity
          -- do
          --    ...;
          --    while ... do
          --        ...;
          --    while ... do
          --        ...;
          -- while ...;
          P.try $ WhileDo <$> (while *> expr) <* act
        ]
    ]

program :: Parser [TopLevel]
program =
  spaceConsumer
    >> P.choice
      [ colon *> decl <&> TopLevelDecl,
        P.try def <&> TopLevelDef,
        expr <&> TopLevelExpr
      ]
      `P.sepEndBy` semi
      <* P.eof

parseLamb :: String -> Text -> Either String [TopLevel]
parseLamb filename = P.parse program filename >>> first P.errorBundlePretty
