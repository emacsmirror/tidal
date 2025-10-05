{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module Mondo.Eval (eval) where

import Sound.Tidal.Core ((#), (|+|))
import Sound.Tidal.Core qualified as T
import Sound.Tidal.Params qualified as T
import Sound.Tidal.ParseBP qualified as T
import Sound.Tidal.Pattern qualified as T
import Sound.Tidal.Scales qualified as T
import Text.Parsec (ParseError)
import Text.Parsec qualified as P
import Text.Parsec.Error qualified as P
import Text.Parsec.Pos qualified as P

import Mondo.Params
import Mondo.Parser
import Mondo.Token

eval :: MondoExpr -> Either ParseError T.ControlPattern
eval (MList xs) = eval_list newEnv xs
eval v = mkError ("expected a list, got: " <> show v) (P.newPos "input" 1 1)

pattern Com :: String -> MondoExpr
pattern Com s <- MPlain (Pos s)

eval_list :: Env -> [MondoExpr] -> Either ParseError T.ControlPattern
eval_list env es = case es of
    Com "s" : rest -> eval_control sPat rest
    -- When notes are at the end, just eval the pattern
    Com "n" : param : [] -> eval_notes param
    -- When notes are after a sound, eval the sound first then apply the notes with |+|
    Com "n" : param : MList rest : [] -> do
        restPat <- eval_list env rest
        notePat <- eval_notes param
        pure $ restPat |+| notePat
    Com "lpf" : rest@(_ : _) -> eval_control lpfPat rest
    Com "fast" : rest@(_ : _) -> eval_mod fastPat rest
    Com "slow" : rest@(_ : _) -> eval_mod slowPat rest
    Com "scale" : param : MList rest : [] -> eval_scale param rest
    MCommand "stack" : rest -> T.stack <$> traverse eval rest
    x : _ -> mkError ("unexpected command: " <> show es) (exprPos x)
    [] -> mkError "expected command!" (P.newPos "input" 1 1)
  where
    eval_notes param = case env.envScale of
        -- No scale defined, eval notes from pattern
        Nothing -> eval_pat env nPat param
        -- Scale was piped, eval int from pattern
        Just scale -> eval_pat env (mkScalePat scale) param

    eval_scale param rest = do
        scalePat <- eval_pat env (mkMondoPat getString) param
        case eval_pat env (mkMondoPat getInt) (MList rest) of
            -- rest are notes, apply the scale and make a control param
            Right notePat -> pure $ T.scale scalePat notePat
            -- rest is probably a pipe, add the scale to the env, and apply it later when encountering notes.
            Left _ -> eval_list (env{envScale = Just (T.scale scalePat)}) rest

    -- Evaluate a control pattern like 's bd'
    eval_control _ [] = error "The impossible has happened!"
    eval_control mondoPat (param : rest) = do
        paramPat <- eval_pat env mondoPat param
        case rest of
            [MList xs] -> do
                restPat <- eval_list env xs
                pure $ mondoPat.combiner paramPat restPat
            [] -> pure paramPat
            -- Here we don't know what's the command, see Note [Chaining Functions Locally]
            [v@(Com _)] | Just currentParam <- env.currentParam -> do
                restPat <- eval_list env [MPlain (Positioned currentParam 0 0), v]
                pure $ mondoPat.combiner paramPat restPat
            other -> mkError ("unexpected control: " <> show other) $ exprPos (MList other)

    -- Evaluate a modifier pattern like 'fast 2'
    eval_mod _ [] = error "The impossible has happened!"
    eval_mod mondoMod (param : rest) = do
        controlPat <- eval_pat env (mkMondoPat mondoMod.exprToPat) param
        restPat <- case rest of
            [MList xs] -> eval_list env xs
            _ -> mkError ("expected command, got: " <> show rest) (exprPos (MList rest))
        pure $ mondoMod.appModifier controlPat restPat

-- Evaluate a 'MondoExpr', according to a 'MondoPat', into a tidal pattern.
eval_pat :: (T.Parseable a, T.Enumerable a) => Env -> MondoPat a b -> MondoExpr -> Either ParseError (T.Pattern b)
eval_pat env mpat expr = case expr of
    -- Use tidal mini notation
    MString p -> case T.parseBP p.value of
        Left err ->
            let errPos = P.errorPos err
                newPos = P.incSourceLine (P.incSourceColumn errPos p.col) p.row
             in Left $ P.setErrorPos newPos err
        Right pat -> pure $ T.withContext (addPos p) (mpat.patToControl pat)
    -- < >
    MList (MCommand "angle" : xs) ->
        T.slow (pure $ toRational $ length xs) . T.timeCat . map (1,) <$> traverse (eval_pat env mpat) xs
    -- [ ]
    MList (MCommand "square" : xs) -> T.fastcat <$> traverse (eval_pat env mpat) xs
    -- x*y
    MList [MCommand "*", param, val] -> eval_op getTime T.fast param val
    -- x:y. How to resolve the result is defined in the 'MondoPat'
    MList [MCommand ":", note, sound] | Just colonOp <- mpat.colonOp -> do
        soundPat <- eval_pat env mpat sound
        notePat <- eval_pat env (mkMondoPat getFloat) note
        pure $ colonOp soundPat notePat
    -- see Note [Chaining Functions Locally]
    MList xs | Just nested <- mpat.nested -> nested newEnv xs
    -- this is a value, make it a pattern.
    _ | Just v <- mpat.exprToPat expr -> pure $ mpat.patToControl v
    _ -> mkError ("unexpected pat: " <> show expr) (exprPos expr)
  where
    eval_op getp appOp param val = do
        paramPat <- eval_pat env (mkMondoPat getp) param
        valPat <- eval_pat env mpat val
        pure $ appOp paramPat valPat

mkError :: String -> P.SourcePos -> Either ParseError a
mkError s = Left . P.newErrorMessage (P.Message s)

exprPos :: MondoExpr -> P.SourcePos
exprPos expr = case expr of
    MPlain v -> posPos v
    MValue v -> posPos v
    MString v -> posPos v
    MList (v : _) -> exprPos v
    _ -> (P.newPos "input" 1 1)
  where
    posPos v = P.newPos "input" v.row v.col

{-
Note [Chaining Functions Locally]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Supporting the feature described in https://strudel.cc/learn/mondo-notation/#chaining-functions-locally
requires a work-around:
The mondo expression: 's [bd hh bd (cp # delay .6)] # bank tr909'
desugar into: '(bank tr909 (s (square bd hh bd (delay .6 cp))))'

When evaluating the very last expression: '(delay .6 cp)', we don't know what 'cp' is.
Thus, the parent control pattern is passed as the 'currentParam' environment, and it is
applied in case a param argument is plain.
-}

nestedCom :: String -> Maybe (Env -> [MondoExpr] -> Either P.ParseError T.ControlPattern)
nestedCom com = Just new_eval
  where
    new_eval env = eval_list (env{currentParam = Just com})

-- * Control Patterns

sPat :: MondoParam String
sPat = MondoPat getString T.sound (Just (\s n -> s |+| T.pF "n" n)) (#) (nestedCom "s")

nPat :: MondoParam T.Note
nPat = MondoPat getNote T.n Nothing (|+|) (nestedCom "n")

lpfPat :: MondoParam Double
lpfPat = MondoPat getDouble T.cutoff Nothing (flip (#)) (nestedCom "lpf")

mkScalePat :: (T.Pattern Int -> T.ControlPattern) -> MondoParam Int
mkScalePat scale = MondoPat getInt scale Nothing (|+|) Nothing

-- * Modifier Patterns

fastPat, slowPat :: MondoMod T.Time
fastPat = MondoMod getTime T.fast
slowPat = MondoMod getTime T.slow
