{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Exception
import Data.Bifunctor
import Data.Either
import Data.Foldable
import Data.List
import Data.Scientific (Scientific)
import Data.Text (Text)
import Lamb.AST
import Lamb.Graphviz
import Lamb.Parser
import Test.Hspec
import qualified Text.Megaparsec as P

parse :: Parser a -> Text -> IO a
parse parser source =
  either (fail . P.errorBundlePretty) return $
    P.parse (parser <* P.eof) "test-src" source

decimals :: [(Text, Expr)]
decimals =
  fmap
    (second Numeral)
    [ ("0", 0),
      ("00000", 0),
      ("1", 1),
      ("0777", 777),
      ("42", 42),
      ("69", 69),
      ("420", 420),
      ("1337", 1337),
      ("14578901250156801206", 14578901250156801206),
      ("0005873290085739199813", 5873290085739199813)
    ]

hexadecimals :: [(Text, Expr)]
hexadecimals =
  fmap
    (second Numeral)
    [ ("0x0", 0),
      ("0x1", 1),
      ("0x000", 0),
      ("0xdeadbeef", 0xdeadbeef),
      ("0x00fa578ba8edc", 0xfa578ba8edc),
      ( "0xcda760ce0da7c60ea7c9076a98076d8576a4c56ae3c576a34c76bbffff",
        0xcda760ce0da7c60ea7c9076a98076d8576a4c56ae3c576a34c76bbffff
      ),
      ("0xDEADBEEF", 0XDEADBEEF),
      ("0x00FA578BA8EDC", 0XFA578BA8EDC),
      ( "0xCDA760CE0DA7C60EA7C9076A98076D8576A4C56AE3C576A34C76BBFFFF",
        0xCDA760CE0DA7C60EA7C9076A98076D8576A4C56AE3C576A34C76BBFFFF
      ),
      ("0X0", 0),
      ("0X1", 1),
      ("0X000", 0),
      ("0XdeaDBEEf", 0xdeadbeef),
      ("0x00fa578ba8edc", 0xfa578ba8edc),
      ( "0xcdA760CE0DA7C60EA7C9076A98076D8576a4c56ae3c576a34c76bbffff",
        0xcda760ce0da7c60ea7c9076a98076d8576a4c56ae3c576a34c76bbffff
      ),
      ("0xDEADBEEF", 0XDEADBEEF),
      ("0x00FA578BA8EDC", 0XFA578BA8EDC),
      ( "0xCDA760CE0DA7C60EA7C9076A98076D8576A4C56AE3C576A34C76BBFFFF",
        0xCDA760CE0DA7C60EA7C9076A98076D8576A4C56AE3C576A34C76BBFFFF
      )
    ]

octals :: [(Text, Expr)]
octals =
  fmap
    (second Numeral)
    [ ("0o0", 0),
      ("0o1", 1),
      ("0o000000", 0),
      ("0o777", 0o777),
      ("0o015627347654", 0o015627347654),
      ( "0o5726134727163472143572143217336271346271534672143176234172347",
        0o5726134727163472143572143217336271346271534672143176234172347
      ),
      ("0O0", 0),
      ("0O1", 1),
      ("0O000000", 0),
      ("0O777", 0O777),
      ("0O015627347654", 0O015627347654),
      ( "0O5726134727163472143572143217336271346271534672143176234172347",
        0O5726134727163472143572143217336271346271534672143176234172347
      )
    ]

bins :: [(Text, Expr)]
bins =
  fmap
    (second Numeral)
    [ ("0b0", 0),
      ("0b1", 1),
      ("0b10", 2),
      ( "0b1010101010000010100110010101010000001011111001010101010100",
        0x2aa0a65502f9554
      )
    ]

floats :: [(Text, Expr)]
floats =
  fmap
    (second Numeral)
    [ ("0.0", 0),
      ("0.12", 0.12),
      ("12.14", 12.14),
      ("3.14", 3.14),
      ("1.618", 1.618),
      ("0.000535e3", 0.000535e3),
      ("0.09853E+3", 0.09853E+3),
      ("0.01e-782935925", read "0.01e-782935925"),
      ( "1035980358.3195891035815e+17777777777",
        read "1035980358.3195891035815e+17777777777"
      ),
      ( "0x12f78eab.a78cde789fdeadbeefp42",
        read "1.399519108947777282687479354604147374629974365234375e21"
      )
    ]

numerals :: [Text]
numerals =
  sort $
    fmap fst floats
      ++ fmap fst bins
      ++ fmap fst octals
      ++ fmap fst hexadecimals
      ++ fmap fst decimals

names :: [(Text, Expr)]
names =
  ((,) <*> Name)
    <$> [ "name",
          "snake_case",
          "camelCase",
          "PascalCase",
          "lowercase",
          "CAPITALCASE",
          "a09183590189515",
          "a_________",
          "a_0498_bVAfadf0_9304",
          "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
        ]

holes :: [(Text, Expr)]
holes =
  fmap
    (,Hole)
    [ "_",
      "__",
      "_0bxcb8935810",
      "_aldjlasdnlkn",
      "___flaf___"
    ]

escapeSequences :: [(Text, Char)]
escapeSequences =
  [ ("\\a", '\a'),
    ("\\b", '\b'),
    ("\\f", '\f'),
    ("\\n", '\n'),
    ("\\r", '\r'),
    ("\\t", '\t'),
    ("\\v", '\v'),
    ("\\\\", '\\'),
    ("\\xff", '\xff'),
    ("\\x80", '\x80'),
    ("\\x00", '\x00'),
    ("\\e", '\x1b'),
    ("\\u0020", '\x20'),
    ("\\u00F7", '÷'),
    ("\\U013254", '𓉔'),
    ("\\ua840", 'ꡀ'),
    ("\\u2118", '℘'),
    ("\\U01D4ab", '𝒫'),
    ("\\uA015", 'ꀕ'),
    ("\\uFe18", '︘')
  ]

chars :: [(Text, Char)]
chars =
  [ ("'\\''", '\''),
    ("'a'", 'a'),
    ("'\\u00f7'", '÷'),
    ("'\\\\'", '\\')
  ]

strings :: [(Text, Expr)]
strings =
  fmap
    (second StringLiteral)
    [ ( "\"A quick brown fox jumps over the lazy dog.\\\"\"",
        "A quick brown fox jumps over the lazy dog.\""
      )
    ]

rawStrings :: [(Text, Expr)]
rawStrings =
  fmap
    (second StringLiteral)
    [ ( "\"\"\" multiline string\n \"\" \" \"\" \r \t \"\"\"",
        " multiline string\n \"\" \" \"\" \r \t "
      )
    ]

lists :: [(Text, Expr)]
lists =
  map
    (second List)
    [ ("[]", []),
      ("[0]", [Numeral 0]),
      ("[0 , ]", [Numeral 0]),
      ( "[   (1, 2)  , (3, 4)  ]",
        [Tuple (Numeral 1) (Numeral 2), Tuple (Numeral 3) (Numeral 4)]
      ),
      ( "[\", ,, ,, ,,,\",45, [ 69, 45, 420 ]]",
        [ StringLiteral ", ,, ,, ,,,",
          Numeral 45,
          List [Numeral 69, Numeral 45, Numeral 420]
        ]
      )
    ]

rangeLists :: [(Text, Expr)]
rangeLists =
  [ ("[-1..]", RangeList (UMinus (Numeral 1)) Nothing Nothing),
    ("[45,46..]", RangeList (Numeral 45) (Just $ Numeral 46) Nothing),
    ( "[\"foo\".. bar]",
      RangeList (StringLiteral "foo") Nothing $ Just $ Name "bar"
    ),
    ("[1,2..50]", RangeList (Numeral 1) (Just $ Numeral 2) (Just $ Numeral 50))
  ]

dicts :: [(Text, Expr)]
dicts =
  map
    (second Dict)
    [ ("{}", []),
      ("{foo: \"bar\"}", [(Name "foo", StringLiteral "bar")]),
      ( "{foo: 42, bar: 69, }",
        [ (Name "foo", Numeral 42),
          (Name "bar", Numeral 69)
        ]
      ),
      ( "{\"\\\\45\\xfe\\\"\": { foo: bar }}",
        [(StringLiteral "\\45\xfe\"", Dict [(Name "foo", Name "bar")])]
      )
    ]

units :: [Text]
units = ["()", "(     )"]

parenExprs :: [(Text, Expr)]
parenExprs =
  [ ("(a)", Name "a"),
    ("((a))", Name "a"),
    ("( (  45 ) )", Numeral 45)
  ]

tuples :: [(Text, Expr)]
tuples =
  [ ("(a, b)", Tuple (Name "a") (Name "b")),
    ("(a, b, c)", Tuple (Name "a") $ Tuple (Name "b") (Name "c"))
  ]

preOps :: [(Text, Expr)]
preOps =
  [ ("(++)", Concatenation Hole Hole),
    ("(   <|  )", RevPipe Hole Hole)
  ]

sections :: [(Text, Expr)]
sections =
  [ ("(a+)", Name "a" `Plus` Hole),
    ("(a * b +)", (Name "a" `Times` Name "b") `Plus` Hole),
    ("(a ++ b +)", (Name "a" `Concatenation` Name "b") `Plus` Hole),
    ("(a + c -)", (Name "a" `Plus` Name "c") `Minus` Hole)
  ]

parenLambdaTerms :: [(Text, Expr)]
parenLambdaTerms =
  [ ("(a -> b)", Lambda $ LambdaDef [(Just "a", Nothing)] (Name "b")),
    ( "(_ b -> c)",
      Lambda $
        LambdaDef
          [ (Nothing, Nothing),
            (Just "b", Nothing)
          ]
          (Name "c")
    ),
    ( "(a : 42 b : int -> c)",
      Lambda $
        LambdaDef
          [ (Just "a", Just $ Pattern Nothing (Numeral 42) Nothing),
            (Just "b", Just $ Pattern Nothing (Name "int") Nothing)
          ]
          (Name "c")
    )
  ]

lambdaTerms :: [(Text, Expr)]
lambdaTerms =
  [ ("\\a -> b", Lambda $ LambdaDef [(Just "a", Nothing)] (Name "b")),
    ( "\\_ b -> c",
      Lambda $
        LambdaDef
          [ (Nothing, Nothing),
            (Just "b", Nothing)
          ]
          (Name "c")
    ),
    ( "\\a : 42 b : int -> c",
      Lambda $
        LambdaDef
          [ (Just "a", Just $ Pattern Nothing (Numeral 42) Nothing),
            (Just "b", Just $ Pattern Nothing (Name "int") Nothing)
          ]
          (Name "c")
    )
  ]

parenTerms :: [Text]
parenTerms = units ++ map fst (parenExprs ++ tuples ++ sections ++ preOps ++ parenLambdaTerms)

terms :: [Text]
terms =
  sort $
    fmap fst holes
      ++ fmap fst names
      ++ numerals
      ++ fmap fst escapeSequences
      ++ fmap fst chars
      ++ fmap fst strings
      ++ fmap fst rawStrings
      ++ fmap fst lists
      ++ fmap fst dicts
      ++ parenTerms
      ++ fmap fst compoundExprs

appExprs :: [(Text, Expr)]
appExprs =
  [ ("a b", Application (Name "a") [Name "b"]),
    ("a b c", Application (Name "a") [Name "b", Name "c"])
  ]

compoundExprs :: [(Text, Expr)]
compoundExprs =
  [ ("{0}", Compound [Numeral 0]),
    ("{0;}", Compound [Numeral 0]),
    ("{0;0}", Compound $ fmap Numeral [0, 0]),
    ("{0;0;}", Compound $ fmap Numeral [0, 0])
  ]

condExprs :: [(Text, Expr)]
condExprs =
  [("if a then b else c", IfThenElse (Name "a") (Name "b") (Name "c"))]

decls :: [(Text, Decl)]
decls =
  [ ("a", Decl ("a", Nothing) []),
    ("a : pat", Decl ("a", Just (Pattern Nothing (Name "pat") Nothing)) []),
    ("a b", Decl ("a", Nothing) [(Just "b", Nothing)]),
    ( "a : pat b : pat",
      Decl
        ("a", Just $ Pattern Nothing (Name "pat") Nothing)
        [(Just "b", Just $ Pattern Nothing (Name "pat") Nothing)]
    )
  ]

pairs ::
  ( Applicative f,
    Foldable f,
    Monoid (f a),
    Monoid (f (a, a))
  ) =>
  f a ->
  f (a, a)
pairs =
  snd
    . foldr
      (\x (xs, acc) -> (pure x <> xs, fmap (x,) xs <> acc))
      (mempty, mempty)

defs :: [(Text, Def)]
defs = fmap (\(t, d) -> (t <> " = e", Def d $ Name "e")) decls

caseExprs :: [(Text, Expr)]
caseExprs =
  [ ( "case a -> b -> c; _ -> d",
      Case
        (Name "a")
        [ (Pattern Nothing (Name "b") Nothing, Name "c"),
          (Pattern Nothing Hole Nothing, Name "d")
        ]
    ),
    ( "case a -> b | c -> d; e -> f",
      Case
        (Name "a")
        [ (Pattern Nothing (Name "b") $ Just (Name "c"), Name "d"),
          (Pattern Nothing (Name "e") Nothing, Name "f")
        ]
    ),
    ( "case a -> b -> c -> d; e -> f",
      Case
        (Name "a")
        [ (Pattern (Just (Name "b")) (Name "c") Nothing, Name "d"),
          (Pattern Nothing (Name "e") Nothing, Name "f")
        ]
    ),
    ( "case a -> b -> c | g -> d; e -> f",
      Case
        (Name "a")
        [ (Pattern (Just (Name "b")) (Name "c") (Just (Name "g")), Name "d"),
          (Pattern Nothing (Name "e") Nothing, Name "f")
        ]
    )
  ]

-- do decl first then test on every pair of decl tests
letExprs :: [(Text, Expr)]
letExprs =
  fmap (\(t, d) -> ("let " <> t <> "; a", Let [d] (Name "a"))) defs
    ++ fmap
      ( \((t1, d1), (t2, d2)) ->
          ( "let "
              <> t1
              <> ";"
              <> t2
              <> "; a",
            Let [d1, d2] (Name "a")
          )
      )
      (pairs defs)

exprs :: [Text]
exprs =
  fmap fst appExprs
    ++ fmap fst condExprs
    ++ fmap fst defs
    ++ fmap fst letExprs
    ++ fmap fst caseExprs
    ++ fmap fst lambdaTerms

noEmpty :: Parser a -> SpecWith (Arg (IO ()))
noEmpty parser =
  it "doesn't accept an empty string" $
    parse parser "" `shouldThrow` anyIOException

notParsing :: (Eq a, Show a) => Parser a -> [Text] -> IO ()
notParsing parser items =
  for_ items $ \s ->
    parse parser s `shouldThrow` anyIOException

infix 4 `notParsing`

parsing :: (Eq a, Show a) => Parser a -> [(Text, a)] -> IO ()
parsing parser items =
  for_ items $ \(s, i) ->
    parse parser s `shouldReturn` i

main :: IO ()
main = hspec $ do
  describe "Lamb.Parser.numeral" $ do
    noEmpty numeral
    it "rejects non-numbers" $ do
      numeral `notParsing` terms \\ numerals
    it "correctly parses decimal integers" $ do
      numeral `parsing` decimals
    it "correctly parses hexadecimal integers" $ do
      parse numeral "0x" `shouldThrow` anyIOException
      parse numeral "0xnan" `shouldThrow` anyIOException
      numeral `parsing` hexadecimals
    it "correctly parses octal integers" $ do
      parse numeral "0o" `shouldThrow` anyIOException
      parse numeral "0o8" `shouldThrow` anyIOException
      numeral `parsing` octals
    it "correctly parses binary integers" $ do
      parse numeral "0b" `shouldThrow` anyIOException
      parse numeral "0b2" `shouldThrow` anyIOException
      numeral `parsing` bins
    it "correctly parses fractional numbers" $ do
      parse numeral "0." `shouldThrow` anyIOException
      parse numeral ".0" `shouldThrow` anyIOException
      parse numeral "." `shouldThrow` anyIOException
      parse numeral "e0" `shouldThrow` anyIOException
      parse numeral "E0" `shouldThrow` anyIOException
      parse numeral "p0" `shouldThrow` anyIOException
      parse numeral "P0" `shouldThrow` anyIOException
      numeral `parsing` floats
  describe "Lamb.Parser.name" $ do
    noEmpty name
    it "rejects non-names" $ do
      name `notParsing` terms \\ map fst names
    it "correctly parses identifiers" $ do
      name `parsing` names
  describe "Lamb.Parser.hole" $ do
    noEmpty hole
    it "rejects non-holes" $ do
      hole `notParsing` terms \\ map fst holes
    it "correctly parses holes" $ do
      hole `parsing` holes
  describe "Lamb.Parser.escapeSequence" $ do
    noEmpty $ escapeSequence '\''
    noEmpty $ escapeSequence '"'
    it "rejects non-escapes" $ do
      escapeSequence '\'' `notParsing` terms \\ map fst escapeSequences
      escapeSequence '"' `notParsing` terms \\ map fst escapeSequences
    it "correctly parses escape sequences" $ do
      escapeSequence '\'' `parsing` escapeSequences
    it "correctly escapes quotes" $ do
      parse (escapeSequence '\'') "\\'" `shouldReturn` '\''
      parse (escapeSequence '"') "\\\"" `shouldReturn` '"'
    it "rejects too large unicode code points" $
      parse (escapeSequence '\'') "\\Uffffff" `shouldThrow` anyIOException
    it "rejects unicode code points with invalid number of hex digits" $ do
      parse (escapeSequence '"') "\\u45" `shouldThrow` anyIOException
      parse (escapeSequence '"') "\\u454545" `shouldThrow` anyIOException
      parse (escapeSequence '"') "\\U1234567" `shouldThrow` anyIOException
      parse (escapeSequence '"') "\\U12345" `shouldThrow` anyIOException
      parse (escapeSequence '"') "\\U1234" `shouldThrow` anyIOException
  describe "Lamb.Parser.stringLiteral" $ do
    noEmpty stringLiteral
    it "rejects non-strings" $ do
      stringLiteral `notParsing` terms \\ map fst strings
    it "correctly parses strings" $ do
      stringLiteral `parsing` strings
  describe "Lamb.Parser.rawStringLiteral" $ do
    noEmpty rawStringLiteral
    it "rejects non-strings" $ do
      rawStringLiteral `notParsing` terms \\ map fst rawStrings
    it "correctly parses raw strings" $ do
      rawStringLiteral `parsing` rawStrings
  describe "Lamb.Parser.list" $ do
    noEmpty list
    it "rejects non-lists" $ do
      list `notParsing` terms \\ (map fst lists ++ map fst rangeLists)
    it "correctly parses lists" $ do
      list `parsing` lists
    it "correctly parses range lists" $ do
      list `parsing` rangeLists
  describe "Lamb.Parser.dict" $ do
    noEmpty dict
    it "rejects non-dicts" $ do
      dict `notParsing` terms \\ map fst dicts
    it "correctly parses dicts" $ do
      dict `parsing` dicts
  describe "Lamb.Parser.parensTerm" $ do
    noEmpty parensTerm
    it "rejects non-parenthesized terms" $ do
      parensTerm `notParsing` terms \\ parenTerms
    it "parses the unit value" $ do
      parensTerm `parsing` fmap (,Unit) units
    it "parses singular expressions" $ do
      parensTerm `parsing` parenExprs
    it "parses tuples" $ do
      parensTerm `parsing` tuples
    it "parses prefix-form operators" $ do
      parensTerm `parsing` preOps
    it "parses postfix sections" $ do
      parensTerm `parsing` sections
    it "parses lambda expressions" $ do
      parensTerm `parsing` parenLambdaTerms
  describe "Lamb.Parser.appTerm" $ do
    noEmpty appTerm
    it "rejects complex non-applicative expressions" $ do
      appTerm `notParsing` exprs \\ fmap fst appExprs
    it "correctly parses applicative expressions" $ do
      appTerm `parsing` appExprs
  describe "Lamb.Parser.compound" $ do
    noEmpty compound
    it "rejects non-compound terms" $ do
      compound `notParsing` terms \\ fmap fst compoundExprs
    it "correctly parses compound terms" $ do
      compound `parsing` compoundExprs
  describe "Lamb.Parser.condTerm" $ do
    noEmpty condTerm
    it "rejects non-conditionals" $ do
      condTerm `notParsing` (exprs ++ terms) \\ fmap fst condExprs
    it "correctly parses conditionals" $
      condTerm `parsing` condExprs
  describe "Lamb.Parser.decl" $ do
    noEmpty decl
    it "correctly parses declarations" $ do
      decl `parsing` decls
  describe "lamb.Parser.def" $ do
    noEmpty def
    it "rejects non-definitions" $ do
      def `notParsing` (exprs ++ terms) \\ fmap fst defs
    it "correctly parses definitions" $ do
      def `parsing` defs
  describe "Lamb.Parser.letTerm" $ do
    noEmpty letTerm
    it "rejects non-lets" $ do
      letTerm `notParsing` (terms ++ exprs) \\ fmap fst letExprs
    it "correctly parses let expressions" $ do
      letTerm `parsing` letExprs
  describe "Lamb.Parser.caseTerm" $ do
    noEmpty caseTerm
    it "rejects non-cases" $ do
      caseTerm `notParsing` (terms ++ exprs) \\ fmap fst caseExprs
    it "correctly parses case expressions" $ do
      caseTerm `parsing` caseExprs
  describe "Lamb.parser.expr" $ do
    noEmpty expr
    it "correctly parses binary operators" $ do
      parse expr "1 + 1" `shouldReturn` Plus (Numeral 1) (Numeral 1)
    it "correctly parses operator associativity" $ do
      parse expr "2 ^ 5 ^ 6" `shouldReturn` Power (Numeral 2) (Power (Numeral 5) (Numeral 6))
    it "correctly parses unary minus" $ do
      parse expr "-2^4" `shouldReturn` UMinus (Power (Numeral 2) (Numeral 4))
    it "correctly parses operator precedence" $ do
      parse expr "2 * -3 + 5" `shouldReturn` Plus (Times (Numeral 2) (UMinus (Numeral 3))) (Numeral 5)
    it "correctly parses non-associative `member of` operator" $ do
      parse expr "a ? 3" `shouldReturn` MemberOf (Name "a") (Numeral 3)
      parse expr "a ? 3 ? 2" `shouldThrow` anyIOException
    it "correctly parses prefix sections" $ do
      parse expr "+2" `shouldReturn` Plus Hole (Numeral 2)
      parse expr "+ 2 * 5" `shouldReturn` Plus Hole (Times (Numeral 2) (Numeral 5))
      parse expr "+ 3 - 2" `shouldReturn` Plus Hole (Minus (Numeral 3) (Numeral 2))
      parse expr "* 2 + 2" `shouldReturn` Times Hole (Plus (Numeral 2) (Numeral 2))
      parse expr "x |> + 42" `shouldReturn` Pipe (Name "x") (Plus Hole (Numeral 42))
      parse expr "a ? b ?> ? c ?> ? d" `shouldReturn` Bind (Bind (MemberOf (Name "a") (Name "b")) (MemberOf Hole (Name "c"))) (MemberOf Hole (Name "d"))
    it "correctly parses complex pipelines" $ do
      -- data |>    (sort &> group &> map length &> sum
      --         <&> find "val" &> || 42               ) |> uncurry (==)
      parse expr "data |> (sort &> group &> map length &> sum <&> find \"val\" &> || 42) |> uncurry (==)"
        `shouldReturn` ( Pipe
                           (Name "data")
                           ( Compose
                               ( Compose
                                   (Compose (Name "sort") (Name "group"))
                                   (Application (Name "map") [Name "length"])
                               )
                               (Name "sum")
                               `Parallel` Compose
                                 (Application (Name "find") [StringLiteral "val"])
                                 (Alternation Hole (Numeral 42))
                           )
                           `Pipe` Application (Name "uncurry") [Equals Hole Hole]
                       )
  describe "Lamb.Parser.lambdaTerm" $ do
    noEmpty lambdaTerm
    it "rejects non-lambdas" $ do
      lambdaTerm `notParsing` exprs \\ map fst lambdaTerms
    it "correctly parses lambdas" $ do
      lambdaTerm `parsing` lambdaTerms

-- TODO: guardTerm, do-while, while-do

-- describe "Lamb.Parser.expr" $ do
--   it "correctly parses various expressions" $ do
-- foldr _ z xs <| \x -> {
--
-- }
-- map ((f _ y) x) : Application (Name "map") (Application (Application (Name "f") Hole) (Name "x"))
-- x |> case _ ->
