{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lamb.Graphviz (programToGraphviz) where

import Control.Arrow ((>>>))
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BSB
import Data.Foldable
import Data.Functor
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Lamb.AST

type GraphvizStep = StateT Int (Writer Builder) Builder

type GraphvizAttribs = [(Builder, Builder)]

graphvizBareAttribList :: GraphvizAttribs -> Builder
graphvizBareAttribList =
  map (\(k, v) -> k <> "=" <> "\"" <> v <> "\"") >>> \case
    [] -> mempty
    x : xs -> x <> foldMap ("," <>) xs

graphvizAttribList :: GraphvizAttribs -> Builder
graphvizAttribList attr = "[" <> graphvizBareAttribList attr <> "]"

plainNode :: (Functor f, Foldable f) => Builder -> GraphvizAttribs -> f GraphvizStep -> GraphvizStep
plainNode label attribs = node label attribs . fmap (,[])

node :: (Foldable f) => Builder -> GraphvizAttribs -> f (GraphvizStep, GraphvizAttribs) -> GraphvizStep
node label attribs =
  foldlM (\acc (w, attr) -> w <&> (: acc) . (,attr)) []
    >=> \childrenLabels -> do
      nodeID <- ("N" <>) . BSB.intDec <$> get
      lift . tell $
        nodeID
          <> graphvizAttribList attribs'
          <> ";"
          <> foldMap
            ( \(c, cattribs) ->
                nodeID
                  <> "->"
                  <> c
                  <> graphvizAttribList cattribs
                  <> ";"
            )
            childrenLabels
      modify (+ 1)
      return nodeID
  where
    attribs' = ("label", label) : attribs

patternToGraphviz :: Pattern -> GraphvizStep
patternToGraphviz (Pattern trans pat cond) =
  node "~" [] $
    toList ((,[("label", "transform")]) . exprToGraphviz <$> trans)
      <> [(exprToGraphviz pat, [("label", "pattern")])]
      <> toList ((,[("label", "condition")]) . exprToGraphviz <$> cond)

argDeclToGraphviz :: (Maybe Text, Maybe Pattern) -> GraphvizStep
argDeclToGraphviz (arg, pat) =
  plainNode "(x)" [] $
    [ case arg of
        Nothing -> exprToGraphviz Hole
        Just nm -> exprToGraphviz $ Name nm
    ]
      <> toList (patternToGraphviz <$> pat)

declToGraphviz :: Decl -> GraphvizStep
declToGraphviz (Decl (nm, pat) args) =
  plainNode "f : x" [] $
    [exprToGraphviz $ Name nm]
      <> toList (patternToGraphviz <$> pat)
      <> map argDeclToGraphviz args

defToGraphviz :: Def -> GraphvizStep
defToGraphviz (Def d e) =
  plainNode "=" [] [declToGraphviz d, exprToGraphviz e]

exprToGraphviz :: Expr -> GraphvizStep
exprToGraphviz = \case
  List l ->
    plainNode "[]" [] $
      map exprToGraphviz l
  RangeList start maybeStep maybeLast ->
    node "[..]" [] $
      [(exprToGraphviz start, [("label", "start")])]
        <> toList ((,[("label", "step")]) . exprToGraphviz <$> maybeStep)
        <> toList ((,[("label", "last")]) . exprToGraphviz <$> maybeLast)
  Dict kv ->
    plainNode "{}" [] $
      map
        ( \(k, v) ->
            plainNode
              "{}"
              []
              [exprToGraphviz k, exprToGraphviz v]
        )
        kv
  Tuple a b ->
    plainNode
      ","
      []
      [exprToGraphviz a, exprToGraphviz b]
  Unit -> plainNode "()" [] []
  Hole -> plainNode "_" [] []
  Name t -> plainNode (T.encodeUtf8Builder t) [] []
  Numeral num -> plainNode (BSB.stringUtf8 $ show num) [] []
  StringLiteral _ -> plainNode "\\\" \\\"" [] []
  CharLiteral _ -> plainNode "' '" [] []
  Compound exprs -> plainNode ";" [] $ fmap exprToGraphviz exprs
  Application f xs -> plainNode "f(x)" [] $ fmap exprToGraphviz $ f NE.<| xs
  NameAccess obj nm -> plainNode "." [] $ map exprToGraphviz [obj, nm]
  IndexAccess obj i -> plainNode "@" [] $ map exprToGraphviz [obj, i]
  Power a b -> plainNode "^" [] $ map exprToGraphviz [a, b]
  UMinus e -> plainNode "-" [] [exprToGraphviz e]
  Times a b -> plainNode "*" [] $ map exprToGraphviz [a, b]
  Divide a b -> plainNode "/" [] $ map exprToGraphviz [a, b]
  Modulo a b -> plainNode "%" [] $ map exprToGraphviz [a, b]
  Plus a b -> plainNode "+" [] $ map exprToGraphviz [a, b]
  Minus a b -> plainNode "-" [] $ map exprToGraphviz [a, b]
  Concatenation a b -> plainNode "++" [] $ map exprToGraphviz [a, b]
  Equals a b -> plainNode "==" [] $ map exprToGraphviz [a, b]
  NotEquals a b -> plainNode "!=" [] $ map exprToGraphviz [a, b]
  Lt a b -> plainNode "<" [] $ map exprToGraphviz [a, b]
  Gt a b -> plainNode ">" [] $ map exprToGraphviz [a, b]
  Le a b -> plainNode "<=" [] $ map exprToGraphviz [a, b]
  Ge a b -> plainNode ">=" [] $ map exprToGraphviz [a, b]
  MemberOf a b -> plainNode "?" [] $ map exprToGraphviz [a, b]
  Conjunction a b -> plainNode "&&" [] $ map exprToGraphviz [a, b]
  Alternation a b -> plainNode "||" [] $ map exprToGraphviz [a, b]
  Compose a b -> plainNode "&>" [] $ map exprToGraphviz [a, b]
  RevCompose a b -> plainNode "<&" [] $ map exprToGraphviz [a, b]
  MonadicCompose a b -> plainNode "^>" [] $ map exprToGraphviz [a, b]
  RevMonadicCompose a b -> plainNode "<^" [] $ map exprToGraphviz [a, b]
  Parallel a b -> plainNode "<&>" [] $ map exprToGraphviz [a, b]
  Pipe a b -> plainNode "|>" [] $ map exprToGraphviz [a, b]
  RevPipe a b -> plainNode "<|" [] $ map exprToGraphviz [a, b]
  Over a b -> plainNode "@>" [] $ map exprToGraphviz [a, b]
  RevOver a b -> plainNode "<@" [] $ map exprToGraphviz [a, b]
  Bind a b -> plainNode "?>" [] $ map exprToGraphviz [a, b]
  RevBind a b -> plainNode "<?" [] $ map exprToGraphviz [a, b]
  Alternative a b -> plainNode "<|>" [] $ map exprToGraphviz [a, b]
  Assign a b -> plainNode ":=" [] $ map exprToGraphviz [a, b]
  PlusAssign a b -> plainNode "+=" [] $ map exprToGraphviz [a, b]
  MinusAssign a b -> plainNode "-=" [] $ map exprToGraphviz [a, b]
  TimesAssign a b -> plainNode "*=" [] $ map exprToGraphviz [a, b]
  DivideAssign a b -> plainNode "/=" [] $ map exprToGraphviz [a, b]
  ModuloAssign a b -> plainNode "%=" [] $ map exprToGraphviz [a, b]
  IfThenElse i t e -> plainNode "if" [] $ map exprToGraphviz [i, t, e]
  Let defs e ->
    plainNode "let" [] $
      fmap defToGraphviz defs <> pure (exprToGraphviz e)
  DoWhile d w ->
    node "do-while" [] $
      fmap ((,[("label", "do")]) . exprToGraphviz) d
        <> pure (exprToGraphviz w, [("label", "while")])
  WhileDo w d ->
    node
      "while-do"
      []
      [ (exprToGraphviz w, [("label", "while")]),
        (exprToGraphviz d, [("label", "do")])
      ]
  Guard conds lastExpr ->
    plainNode "|" [] $
      fmap
        (\(cond, e) -> plainNode "|->" [] $ fmap exprToGraphviz [cond, e])
        conds
        <> pure (plainNode "otherwise" [] [exprToGraphviz lastExpr])
  SingleGuard cond e -> plainNode "|->" [] $ fmap exprToGraphviz [cond, e]
  Case obj pats ->
    plainNode "case" [] $
      pure (exprToGraphviz obj)
        <> fmap
          ( \(pat, e) ->
              plainNode
                "->|->"
                []
                [patternToGraphviz pat, exprToGraphviz e]
          )
          pats
  Lambda (LambdaDef args e) ->
    plainNode "\\\\" [] $
      fmap argDeclToGraphviz args <> pure (exprToGraphviz e)

topLevelToGraphviz :: TopLevel -> GraphvizStep
topLevelToGraphviz = \case
  TopLevelExpr e -> exprToGraphviz e
  TopLevelDecl d -> declToGraphviz d
  TopLevelDef d -> defToGraphviz d

programToGraphviz :: GraphvizAttribs -> GraphvizAttribs -> GraphvizAttribs -> [TopLevel] -> Builder
programToGraphviz gAttribs nAttribs eAttribs toplevel =
  "strict digraph{"
    <> foldMap (\(k, v) -> k <> "=" <> "\"" <> v <> "\"" <> ";") gAttribs
    <> ("node" <> graphvizAttribList nAttribs <> ";")
    <> ("edge" <> graphvizAttribList eAttribs <> ";")
    <> prog
    <> "}"
  where
    prog =
      execWriter $
        evalStateT (plainNode "program" [] (map topLevelToGraphviz toplevel)) 0
