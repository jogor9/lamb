{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Lamb.Eval (valueToString, liftIO, runEval, runEvalStep, evalTopLevel, evalExpr) where

import Control.Applicative
import Control.Arrow ((>>>))
-- TODO: handle io exceptions
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Bifunctor
import qualified Data.ByteString as BS
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Traversable
import Lamb.AST
import Text.Printf

newtype EvalState = EvalState
  {_bindings :: HashMap Text (NonEmpty Value)}

type EvalT = StateT EvalState

type EvalStepT m = ExceptT Text (EvalT m)

data Value
  = UnitValue
  | BooleanValue Bool
  | NumericValue Double
  | CharValue Char
  | StringValue Text
  | TupleValue Value Value
  | ListValue [Value]
  | DictValue (HashMap Text Value)
  | Function (Value -> IO Value)

makeLenses ''EvalState

modFloat :: (RealFrac a) => a -> a -> a
modFloat e d = e - fromIntegral (floor $ e / d :: Int) * d

valueCompare ::
  (MonadIO m) =>
  Text ->
  Value ->
  Value ->
  EvalStepT m Ordering
valueCompare context a' b' = case (a', b') of
  (UnitValue, UnitValue) -> return EQ
  (BooleanValue a, BooleanValue b) -> return $ compare a b
  (NumericValue a, NumericValue b) -> return $ compare a b
  (CharValue a, CharValue b) -> return $ compare a b
  (StringValue a, StringValue b) -> return $ compare a b
  (TupleValue a b, TupleValue c d) ->
    (,)
      <$> valueCompare context a c
      <*> valueCompare context b d
      <&> \case
        (EQ, cmp) -> cmp
        (cmp, _) -> cmp
  (ListValue a, ListValue b) ->
    zipWithM (valueCompare context) a b
      <&> ( dropWhile (== EQ)
              >>> \case
                [] -> compare (length a) (length b)
                h : _ -> h
          )
  (DictValue a, DictValue b) ->
    case compare (Map.keys a) (Map.keys b) of
      EQ ->
        Map.intersectionWith (valueCompare context) a b
          & sequence
          <&> ( dropWhile (== EQ) . toList
                  >>> \case
                    [] -> EQ
                    h : _ -> h
              )
      cmp -> return cmp
  _ -> throwE $ "Comparison between incompatible objects " <> context

evalTypePattern ::
  (MonadIO m) =>
  Value ->
  Maybe Expr ->
  Expr ->
  Maybe Expr ->
  EvalStepT m ()
evalTypePattern _ _ _ _ = throwE "Type patterns not implemented"

evalDef :: (MonadIO m) => Def -> EvalStepT m ()
evalDef (Def (Decl (nm, _type) args) expr) = case args of
  [] -> evalExpr expr >>= bindName nm
  h : t -> evalExpr (Lambda (h :| t) expr) >>= bindName nm

-- matches value against pattern expression,
-- returns Just all of the names bound if the pattern matched
-- or Nothing if the pattern didn't match
evalPattern :: Value -> Expr -> Maybe [(Text, Value)]
evalPattern v (Name nm) = Just [(nm, v)]
evalPattern _ Hole = Just []
evalPattern UnitValue Unit = Just []
evalPattern (BooleanValue b) (BoolLiteral b')
  | b == b' = Just []
  | otherwise = Nothing
evalPattern (NumericValue n) (Numeral n')
  | n == toRealFloat n' = Just []
  | otherwise = Nothing
evalPattern (CharValue c) (CharLiteral c')
  | c == c' = Just []
  | otherwise = Nothing
evalPattern (StringValue s) (StringLiteral s')
  | s == s' = Just []
  | otherwise = Nothing
evalPattern (TupleValue a b) (Tuple a' b') =
  (<>) <$> evalPattern a a' <*> evalPattern b b'
evalPattern (ListValue l) (Lamb.AST.List l') =
  zipWithM evalPattern l l' <&> concat
evalPattern (DictValue d) (Dict d') =
  if not . null $ Map.difference d' d
    then Nothing
    else Map.intersectionWith evalPattern d d' & sequence <&> concat
evalPattern _ _ = Nothing

bindName :: (MonadIO m) => Text -> Value -> EvalStepT m ()
bindName k v = lift $ bindings %= Map.insertWith (<>) k (v :| [])

collectHoles :: Expr -> (Expr, [Expr])
collectHoles = second fst . flip runState ([], 0 :: Int) . collectHoles'
  where
    collectHoles' = \case
      Hole -> nameHole
      Lamb.AST.List l -> l & traverse collectHoles' <&> Lamb.AST.List
      RangeList start step end ->
        RangeList
          <$> collectHoles' start
          <*> traverse collectHoles' step
          <*> traverse collectHoles' end
      Dict d ->
        d
          & traverse collectHoles'
          <&> Dict
      Tuple a b -> Tuple <$> collectHoles' a <*> collectHoles' b
      Compound exprs -> exprs & traverse collectHoles' <&> Compound
      Application fun args ->
        Application <$> evalHole fun <*> traverse evalHole args
      NameAccess a b -> NameAccess <$> collectHoles' a <*> collectHoles' b
      IndexAccess a b -> IndexAccess <$> collectHoles' a <*> collectHoles' b
      Power a b -> Power <$> collectHoles' a <*> collectHoles' b
      UMinus a -> UMinus <$> collectHoles' a
      Times a b -> Times <$> collectHoles' a <*> collectHoles' b
      Divide a b -> Divide <$> collectHoles' a <*> collectHoles' b
      Modulo a b -> Modulo <$> collectHoles' a <*> collectHoles' b
      Plus a b -> Plus <$> collectHoles' a <*> collectHoles' b
      Minus a b -> Minus <$> collectHoles' a <*> collectHoles' b
      Concatenation a b -> Concatenation <$> collectHoles' a <*> collectHoles' b
      Equals a b -> Equals <$> collectHoles' a <*> collectHoles' b
      NotEquals a b -> NotEquals <$> collectHoles' a <*> collectHoles' b
      Lt a b -> Lt <$> collectHoles' a <*> collectHoles' b
      Gt a b -> Gt <$> collectHoles' a <*> collectHoles' b
      Le a b -> Le <$> collectHoles' a <*> collectHoles' b
      Ge a b -> Ge <$> collectHoles' a <*> collectHoles' b
      MemberOf a b -> MemberOf <$> collectHoles' a <*> collectHoles' b
      Conjunction a b -> Conjunction <$> collectHoles' a <*> collectHoles' b
      Alternation a b -> Alternation <$> collectHoles' a <*> collectHoles' b
      Compose a b -> Compose <$> evalHole a <*> evalHole b
      MonadicCompose a b -> MonadicCompose <$> evalHole a <*> evalHole b
      RevCompose a b -> RevCompose <$> evalHole a <*> evalHole b
      RevMonadicCompose a b -> RevMonadicCompose <$> evalHole a <*> evalHole b
      Parallel a b -> Parallel <$> evalHole a <*> evalHole b
      IfThenElse a b c ->
        IfThenElse
          <$> collectHoles' a
          <*> collectHoles' b
          <*> collectHoles' c
      Let defs e -> Let defs <$> collectHoles' e
      DoWhile exprs cond ->
        DoWhile
          <$> traverse collectHoles' exprs
          <*> collectHoles' cond
      WhileDo cond expr -> WhileDo <$> collectHoles' cond <*> collectHoles' expr
      Guard guards def ->
        Guard
          <$> traverse
            ( \(c, e) ->
                (,)
                  <$> collectHoles' c
                  <*> collectHoles' e
            )
            guards
          <*> collectHoles' def
      IfDo cond expr -> IfDo <$> collectHoles' cond <*> collectHoles' expr
      Case v pats -> flip Case pats <$> collectHoles' v
      Pipe a b -> Pipe <$> collectHoles' a <*> evalHole b
      Bind a b -> Bind <$> collectHoles' a <*> evalHole b
      Over a b -> Over <$> collectHoles' a <*> evalHole b
      RevPipe a b -> RevPipe <$> collectHoles' a <*> evalHole b
      RevBind a b -> RevBind <$> collectHoles' a <*> evalHole b
      RevOver a b -> RevOver <$> collectHoles' a <*> evalHole b
      Alternative a b -> Alternative <$> collectHoles' a <*> collectHoles' b
      Assign a b -> Assign <$> collectHoles' a <*> collectHoles' b
      PlusAssign a b -> PlusAssign <$> collectHoles' a <*> collectHoles' b
      MinusAssign a b -> MinusAssign <$> collectHoles' a <*> collectHoles' b
      TimesAssign a b -> TimesAssign <$> collectHoles' a <*> collectHoles' b
      DivideAssign a b -> DivideAssign <$> collectHoles' a <*> collectHoles' b
      ModuloAssign a b -> ModuloAssign <$> collectHoles' a <*> collectHoles' b
      term -> return term
      where
        evalHole = \case
          Hole -> nameHole
          expr -> return expr
        nameHole = do
          i <- get <&> snd
          let holeName = Name . T.pack $ printf "_%d" i
          _1 %= (holeName :)
          _2 %= (+ 1)
          return holeName

evalBoolean :: (MonadIO m) => Text -> Expr -> EvalStepT m Bool
evalBoolean context =
  evalExpr >=> \case
    BooleanValue b -> return b
    _ -> throwE $ "Expected a boolean " <> context

evalArgument :: (MonadIO m) => Expr -> EvalStepT m Expr
evalArgument = \case
  Hole -> throwE "Holes not allowed on top Level"
  expr ->
    collectHoles expr
      & \(rexpr, holeNames) ->
        return $ case holeNames of
          [] -> rexpr
          h : hs ->
            h :| hs
              & NE.reverse
              & fmap (\nm -> ((Nothing, nm, Nothing) :| [], Nothing))
              & flip Lambda rexpr

evalFunction ::
  (MonadIO m) =>
  Text ->
  Expr ->
  EvalStepT m (Value -> IO Value)
evalFunction context =
  evalArgument
    >=> evalExpr
    >=> \case
      Function f -> return f
      _ -> throwE $ "Expected a function " <> context

evalPatternClause ::
  (MonadIO m) =>
  Value ->
  Maybe Expr ->
  Expr ->
  Maybe Expr ->
  EvalStepT m Bool
evalPatternClause v t p g = do
  fun <-
    evalFunction "in pattern transformation clause" $
      fromMaybe (Name "id") t
  m <- liftIO $ fun v
  case evalPattern m p of
    Just bs -> do
      traverse_ (uncurry bindName) bs
      evalBoolean "in pattern guard clause" $
        fromMaybe (BoolLiteral True) g
    Nothing -> return False

evalMultiPattern ::
  (MonadIO m) =>
  Value ->
  NonEmpty (Maybe Expr, Expr, Maybe Expr) ->
  EvalStepT m Bool
evalMultiPattern v ((t, p, g) :| []) = evalPatternClause v t p g
evalMultiPattern (TupleValue v v') ((t, p, g) :| r : rs) =
  (&&) <$> evalPatternClause v t p g <*> evalMultiPattern v' (r :| rs)
evalMultiPattern _ _ = return False

evalExpr :: (MonadIO m) => Expr -> EvalStepT m Value
evalExpr = \case
  Lamb.AST.List l -> ListValue <$> traverse evalExpr l
  RangeList _start _step _end -> throwE "Range lists not implemented"
  Dict d -> DictValue <$> traverse evalExpr d
  Tuple a b -> TupleValue <$> evalExpr a <*> evalExpr b
  Unit -> return UnitValue
  Hole -> throwE "Holes not allowed on top level"
  Name nm ->
    lift get >>= \case
      ((^. bindings) >>> Map.lookup nm -> Just (v :| _)) -> return v
      _ -> throwE . T.pack $ printf "Name '%s' is not bound to any variable" nm
  Numeral n -> return $ NumericValue $ toRealFloat n
  BoolLiteral b -> return $ BooleanValue b
  StringLiteral s -> return $ StringValue s
  CharLiteral c -> return $ CharValue c
  Compound es -> runCompound es <&> NE.last
  -- one way to do holes is to transform the parent expression to a lambda,
  -- such that:
  -- if _ == 0 then x |> _ else y ?> _
  -- becomes
  -- if (n -> n == 0) then (f -> x |> f) else (m -> y ?> m)
  -- because an application takes all of its arguments at once it is also
  -- possible to transform:
  -- foldr _ _ xs
  -- into
  -- (f z -> foldr f z xs)
  -- transforming the following expression:
  -- 3 * 4 / _
  -- would yield 3 * (x -> 4 / x)
  -- which is semantically incorrect
  -- (the `if` expression in the first example is also semantically invalid)
  --
  -- transforming up to the first parent application, composition or pipe
  -- would make
  -- map (if _ == 0 then x |> _ else y ?> _)
  -- yield
  -- map (n -> if n == 0 then (f -> x |> f) else (m -> y ?> m))
  -- (a semantically correct and desirable choice)
  --
  -- map (3 * 4 / _)
  -- would yield
  -- map (x -> 3 * 4 / x)
  --
  -- map (2 * length _)
  -- would yield
  -- map (l -> 2 * length l)
  --
  -- map (2 * _.length)
  -- would yield
  -- map (l -> 2 * l.length)
  --
  -- f |> _ a b
  -- yields
  -- f |> (f -> f a b)
  --
  -- map (x |> _)
  -- yields
  -- map (f -> x |> f)
  --
  -- Pipes treat the left argument normally, e.g.
  -- map (length _ |> f)
  -- becomes
  -- map (l -> length l |> f)
  --
  -- Compositions (and applications) treat both arguments specially, e.g:
  -- map (length _ &> _ / 2)
  -- becomes
  -- map ((l -> length) &> (x -> x / 2))
  -- (note that this same expression could be written as)
  -- map (length _ / 2) ---> map (l -> length l / 2)
  -- (a potential upgrade would stop evaluating application arguments only when
  -- the function were known to accept a higher order function. I'm not sure
  -- whether or not this would even be decideable though)
  --
  --
  -- holes in a lambda are added as if they were additional arguments in the
  -- lambda, e.g. (l -> foldr _ _ l) becomes (l f z -> foldr f z l)

  -- here we evaluate holes
  Application e es -> do
    f <- evalArgument e >>= evalExpr
    args <- traverse evalArgument es
    foldlM
      ( \case
          Function f' -> evalExpr >=> liftIO . f'
          _ -> const $ throwE "Invalid number of arguments to a function"
      )
      f
      args
  NameAccess obj nm -> do
    d <- evalDict "in member access expression" obj
    s <- evalName "in member access expression" nm
    Map.lookup s d
      & maybe
        ( throwE . T.pack $
            printf "Name '%s' is not part of the given object" s
        )
        return
  IndexAccess obj idx -> do
    i :: Int <-
      evalNumber "in index access expression" idx
        >>= \case
          i | i < 0 -> throwE "Negative index"
          i | i >= fromIntegral (maxBound :: Int) -> throwE "Index too large"
          i -> return $ floor i
    evalExpr obj >>= \case
      ListValue l -> case drop i l of
        [] -> throwE "List index access out of bounds"
        x : _ -> return x
      TupleValue a' a'' ->
        let it a b = \case
              0 -> return a
              n -> case b of
                TupleValue x y -> it x y (n - 1)
                x ->
                  if n == 1
                    then return x
                    else throwE "Tuple index access out of bounds"
         in it a' a'' i
      _ -> throwE "Expected a tuple or a list in index access expression"
  Power a b ->
    let ctx = "in power expression"
     in (**) <$> evalNumber ctx a <*> evalNumber ctx b <&> NumericValue
  UMinus a ->
    negate <$> evalNumber "in unary minus expression" a <&> NumericValue
  Times a b ->
    let ctx = "in times expression"
     in (*) <$> evalNumber ctx a <*> evalNumber ctx b <&> NumericValue
  Divide a b ->
    let ctx = "in divide expression"
     in (/) <$> evalNumber ctx a <*> evalNumber ctx b <&> NumericValue
  Modulo a b ->
    let ctx = "in modulo expression"
     in modFloat
          <$> evalNumber ctx a
          <*> evalNumber ctx b
          <&> NumericValue
  Plus a b ->
    let ctx = "in plus expression"
     in (+) <$> evalNumber ctx a <*> evalNumber ctx b <&> NumericValue
  Minus a b ->
    let ctx = "in minus expression"
     in (-) <$> evalNumber ctx a <*> evalNumber ctx b <&> NumericValue
  Concatenation a b ->
    let ctx = "in concatenation expression"
     in (++) <$> evalList ctx a <*> evalList ctx b <&> ListValue
  Equals a b ->
    evalComparison "in equals expression" a b <&> BooleanValue . (== EQ)
  NotEquals a b ->
    evalComparison "in not equals expression" a b <&> BooleanValue . (/= EQ)
  Lt a b ->
    evalComparison "in less than expression" a b <&> BooleanValue . (== LT)
  Gt a b ->
    evalComparison "in greater than expression" a b <&> BooleanValue . (== GT)
  Le a b ->
    evalComparison "in less than or equal to expression" a b
      <&> BooleanValue . (/= GT)
  Ge a b ->
    evalComparison "in greater than or equal to expression" a b
      <&> BooleanValue . (/= LT)
  MemberOf a b ->
    let ctx = "in member of expression"
     in flip Map.lookup
          <$> evalDict ctx a
          <*> evalName ctx b
          <&> ListValue . toList
  Conjunction a b ->
    let ctx = "in conjunction expression"
     in (&&) <$> evalBoolean ctx a <*> evalBoolean ctx b <&> BooleanValue
  Alternation a b ->
    let ctx = "in alternation expression"
     in (||) <$> evalBoolean ctx a <*> evalBoolean ctx b <&> BooleanValue
  Compose a b ->
    let ctx = "in composition expression"
     in (>=>) <$> evalFunction ctx a <*> evalFunction ctx b <&> Function
  MonadicCompose a b ->
    let ctx = "in monadic composition expression"
     in (\f g x -> f x >>= traverse g <&> ListValue . join)
          <$> evalMonadic ctx a
          <*> evalMonadic ctx b
          <&> Function
  RevCompose a b ->
    let ctx = "in reverse composition expression"
     in (<=<) <$> evalFunction ctx a <*> evalFunction ctx b <&> Function
  RevMonadicCompose a b ->
    let ctx = "in reverse monadic composition expression"
     in (\g f x -> f x >>= traverse g <&> ListValue . join)
          <$> evalMonadic ctx a
          <*> evalMonadic ctx b
          <&> Function
  Parallel a b ->
    let ctx = "in parallel expression"
     in (\f g x -> TupleValue <$> f x <*> g x)
          <$> evalFunction ctx a
          <*> evalFunction ctx b
          <&> Function
  IfThenElse a b c ->
    (\cond t e -> if cond then t else e)
      <$> evalBoolean "in if condition" a
      <*> evalExpr b
      <*> evalExpr c
  Let defs expr ->
    runScoped $
      traverse_ evalDef defs >> evalExpr expr
  DoWhile body cond ->
    let it = do
          _ <- runCompound body
          condResult <- evalBoolean "in do-while loop condition" cond
          if condResult
            then it
            else return UnitValue
     in it
  WhileDo cond body ->
    let it = do
          condResult <- evalBoolean "in a while-do loop condition" cond
          if condResult
            then evalExpr body >> it
            else return UnitValue
     in it
  Guard guards z ->
    let it [] = evalExpr z
        it ((g, e) : gs) = do
          r <- evalBoolean "in a guard condition clause" g
          if r
            then
              evalExpr e
            else
              it gs
     in it (toList guards)
  IfDo cond body -> do
    r <- evalBoolean "in an if-do condition clause" cond
    if r
      then
        ListValue . pure <$> evalExpr body
      else
        return $ ListValue []
  Case targetExpr pats ->
    evalExpr targetExpr >>= \targetVal ->
      let it [] = throwE "Case expression without exhaustive patterns"
          it ((f, p, g, e) : ps) =
            evalPatternClause targetVal f p g >>= \case
              True -> evalExpr e
              False -> it ps
       in runScoped . it $ toList pats
  Pipe x f ->
    (&)
      <$> evalExpr x
      <*> evalFunction "in pipe expression" f
      >>= liftIO
  Bind x f ->
    monadicApply
      <$> evalList "in bind expression" x
      <*> evalMonadic "in bind expression" f
      >>= liftIO
  Over x f ->
    for
      <$> evalList "in over expression" x
      <*> evalFunction "in over expression" f
      >>= liftIO . fmap ListValue
  RevPipe f x ->
    ($)
      <$> evalFunction "in reverse pipe expression" f
      <*> evalExpr x
      >>= liftIO
  RevBind f x ->
    flip monadicApply
      <$> evalMonadic "in reverse bind expression" f
      <*> evalList "in reverse bind expression" x
      >>= liftIO
  RevOver f x ->
    traverse
      <$> evalFunction "in reverse over expression" f
      <*> evalList "in reverse over expression" x
      >>= liftIO . fmap ListValue
  Alternative a b ->
    (<|>)
      <$> evalList "in alternative expression" a
      <*> evalList "in alternative expression" b
      <&> ListValue
  Assign k' v' -> do
    k <- evalName "in assignment expression" k'
    v <- evalExpr v'
    b <- lift $ get <&> (^. bindings)
    unless (k `Map.member` b) $
      throwE "Name is not defined"
    bindings %= Map.adjust (\(_ :| vs) -> v :| vs) k
    return UnitValue
  PlusAssign k v -> modNumber (+) k v
  MinusAssign k v -> modNumber (-) k v
  TimesAssign k v -> modNumber (*) k v
  DivideAssign k v -> modNumber (/) k v
  ModuloAssign k v -> modNumber modFloat k v
  -- Lambdas capture their environment by value
  Lambda ((multiPat, _type) :| args) expr -> do
    b <- lift get
    return $ Function $ \v -> (\l -> l >>= either (fail . T.unpack) return)
      . flip evalStateT b
      . runExceptT
      $ do
        evalMultiPattern v multiPat >>= \case
          False -> throwE "unmatched pattern in lambda expression"
          True -> case args of
            [] -> evalArgument expr >>= evalExpr
            h : t -> evalExpr $ Lambda (h :| t) expr
  where
    modNumber f k' v' = do
      k <- evalName "in assignment expression" k'
      v <- evalNumber "in assignment expression" v'
      lift get >>= \case
        ((^. bindings) >>> Map.lookup k -> Just (NumericValue v'' :| _)) -> do
          bindings %= Map.adjust (\(_ :| vs) -> NumericValue (f v'' v) :| vs) k
          return UnitValue
        _ -> throwE "Name is not defined"
    monadicApply :: [Value] -> (Value -> IO [Value]) -> IO Value
    monadicApply x f = traverse f x <&> ListValue . join
    runCompound exprs = do
      runScoped $ for exprs $ \case
        Let defs expr -> traverse_ evalDef defs >> evalExpr expr
        expr -> evalExpr expr
    runScoped expr = do
      saved <- lift get
      result <- lift $ runExceptT expr
      case result of
        Left e -> do
          lift $ put saved
          throwE e
        Right s -> do
          -- remove all bindings that were defined in this scope,
          -- but keep the modified variables
          lift $
            bindings %= \current ->
              Map.intersectionWith
                (\c p -> if length c > length p then p else c)
                current
                (saved ^. bindings)
          return s
    evalMonadic context =
      evalArgument
        >=> evalExpr
        >=> \case
          Function f ->
            return $
              f >=> \case
                ListValue l -> return l
                _ -> fail $ "Expected a monadic function " <> T.unpack context
          _ -> throwE $ "Expected a function " <> context
    evalComparison context a' b' =
      (,)
        <$> evalExpr a'
        <*> evalExpr b'
        >>= uncurry (valueCompare context)

    evalList context =
      evalExpr >=> \case
        ListValue l -> return l
        _ -> throwE $ "Expected a list " <> context
    evalDict context =
      evalExpr >=> \case
        DictValue d -> return d
        _ -> throwE $ "Expected a dictionary " <> context
    evalName context = \case
      Name s -> return s
      e ->
        evalExpr e >>= \case
          StringValue s -> return s
          _ -> throwE $ "Expected a name " <> context
    evalNumber context =
      evalExpr >=> \case
        NumericValue num -> return num
        _ -> throwE $ "Expected a number " <> context

evalTopLevel :: (MonadIO m) => TopLevel -> EvalStepT m (Either Text Value)
evalTopLevel = \case
  TopLevelDef d@(Def (Decl (nm, _) _) _) -> evalDef d >> return (Left nm)
  TopLevelExpr e -> Right <$> evalExpr e
  TopLevelDecl _ -> error "declarations not implemented"

valueToString :: Value -> String
valueToString = \case
  UnitValue -> "()"
  BooleanValue True -> "true"
  BooleanValue False -> "false"
  NumericValue n -> show n
  CharValue c -> [c]
  StringValue s -> T.unpack s
  TupleValue a b ->
    let it = \case
          TupleValue a' b' -> "," ++ valueToString a' ++ it b'
          v -> "," ++ valueToString v
     in "(" ++ valueToString a ++ it b ++ ")"
  ListValue l -> "[" ++ intercalate "," (fmap valueToString l) ++ "]"
  DictValue d ->
    "{"
      ++ intercalate
        ","
        ( (\(k, v) -> T.unpack k ++ ":" ++ valueToString v)
            <$> Map.toList d
        )
      ++ "}"
  Function _ -> "<FUNCTION>"

function :: (Value -> IO Value) -> IO Value
function = return . Function

stringFunction :: (Text -> IO Value) -> IO Value
stringFunction f = function $ \case
  StringValue s -> f s
  _ -> fail "expected a string"

builtins :: IO (HashMap Text Value)
builtins =
  sequence $
    Map.fromList
      [ ( "print",
          return . Function $
            (UnitValue <$) . putStrLn . valueToString
        ),
        ( "readFile",
          stringFunction $ \s ->
            StringValue . T.decodeUtf8 <$> BS.readFile (T.unpack s)
        ),
        ( "writeFile",
          stringFunction
            ( \file -> stringFunction $ \content ->
                UnitValue <$ T.writeFile (T.unpack file) content
            )
        )
        -- TODO: figure out how to pass stdin
        -- ( "readInput",
        --   StringValue . T.pack <$> getContents
        -- ),
        -- ( "writeOutput",
        --   stringFunction $ \content -> UnitValue <$ T.putStr content
        -- )
      ]

runEvalStep :: EvalStepT m a -> EvalT m (Either Text a)
runEvalStep = runExceptT

runEval :: (MonadIO m) => EvalT m () -> m ()
runEval eval = liftIO builtins >>= evalStateT eval . EvalState . fmap (:| [])
