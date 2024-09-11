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

sections :: [(Text, Expr)]
sections =
  [ ("(a+)", Name "a" `Plus` Hole),
    ("(a * b +)", (Name "a" `Times` Name "b") `Plus` Hole),
    ("(a ++ b +)", (Name "a" `Concatenation` Name "b") `Plus` Hole),
    ("(a + c -)", (Name "a" `Plus` Name "c") `Minus` Hole)
  ]

parenTerms :: [Text]
parenTerms = units ++ map fst (parenExprs ++ tuples ++ sections)

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
    it "parses postfix sections" $ do
      parensTerm `parsing` sections
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
    it "rejects non-conditional terms" $ do
      condTerm `notParsing` terms \\ fmap fst condExprs
    it "correctly parses conditionals" $
      condTerm `parsing` condExprs
  describe "Lamb.Parser.decl" $ do
    noEmpty decl
    it "correctly parses declarations" $ do
      decl `parsing` decls
  describe "lamb.Parser.def" $ do
    noEmpty def
    it "rejects non-definitions" $ do
      def `notParsing` exprs ++ terms
    it "correctly parses definitions" $ do
      def `parsing` defs
  describe "Lamb.Parser.letTerm" $ do
    noEmpty letTerm
    it "rejects non-lets" $ do
      letTerm `notParsing` (terms ++ exprs) \\ fmap fst letExprs
    it "correctly parses let expressions" $ do
      letTerm `parsing` letExprs

-- describe "Lamb.Parser.expr" $ do
--   it "correctly parses various expressions" $ do
-- foldr _ z xs <| \x -> {
--
-- }
-- map ((f _ y) x) : Application (Name "map") (Application (Application (Name "f") Hole) (Name "x"))
-- x |> case _ ->
