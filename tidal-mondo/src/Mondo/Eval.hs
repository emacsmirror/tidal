{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module Mondo.Eval where

import GHC.Float (float2Double)

import Sound.Tidal.Core ((#))
import Sound.Tidal.Core qualified as T
import Sound.Tidal.Params qualified as T
import Sound.Tidal.ParseBP qualified as T
import Sound.Tidal.Pattern qualified as T
import Text.Parsec (ParseError)
import Text.Parsec.Error qualified as P
import Text.Parsec.Pos qualified as P

import Mondo.Parser
import Mondo.Token

eval :: MondoExpr -> Either ParseError T.ControlPattern
eval (MList xs) = eval_list xs
eval v = mkError ("expected a list, got: " <> show v) (P.newPos "input" 1 1)

pattern Com :: String -> MondoExpr
pattern Com s <- MPlain (Pos s)

data TidalPat a
    = Before (T.Pattern a -> T.ControlPattern -> T.ControlPattern)
    | After (T.Pattern a -> T.ControlPattern)

eval_list :: [MondoExpr] -> Either ParseError T.ControlPattern
eval_list xs = case xs of
    (Com "s" : rest) -> T.fastCat <$> traverse (resolve_seq "s" T.sound getString) rest
    (Com "lpf" : xs) -> eval_control getDouble (After T.cutoff) xs
    (Com "fast" : xs) -> eval_control getTime (Before T.fast) xs
    (Com "slow" : xs) -> eval_control getTime (Before T.slow) xs
    x : _ -> mkError ("unexpected command: " <> show xs) (exprPos x)
    [] -> mkError "expected command!" (P.newPos "input" 1 1)
  where
    eval_control get app xs = case (init xs, last xs) of
        (params, MList rest) -> do
            paramPat <- T.fastCat <$> traverse (eval_pat get) params
            controlPat <- eval_list rest
            pure $ case app of
                After app -> controlPat # app paramPat
                Before app -> app paramPat controlPat
        _ -> mkError ("invalid control pattern: " <> show xs) (exprPos $ head xs)
eval_pat :: (T.Parseable a, T.Enumerable a) => (MondoExpr -> Maybe (Positioned a)) -> MondoExpr -> Either ParseError (T.Pattern a)
eval_pat get expr = case expr of
    (MString p) -> case T.parseBP p.value of
        Left err ->
            let errPos = P.errorPos err
                newPos = P.incSourceLine (P.incSourceColumn errPos p.col) p.row
             in Left $ P.setErrorPos newPos err
        Right pat -> pure $ T.withContext (addPos p) pat
    (MList (MCommand "angle" : xs)) ->
        T.slow (pure $ toRational $ length xs) . T.timeCat . map (1,) <$> traverse (eval_pat get) xs
    (MList (MCommand "square" : xs)) -> T.fastcat <$> traverse (eval_pat get) xs
    (MList [MCommand "*", param, val]) -> eval_op getTime T.fast param val
    _ -> case get expr of
        Just v -> Right (patWithPos v)
        Nothing -> mkError ("unexpected pat: " <> show expr) (exprPos expr)
  where
    eval_op getp app param val = do
        paramPat <- eval_pat getp param
        valPat <- eval_pat get val
        pure $ app paramPat valPat

getDouble :: MondoExpr -> Maybe (Positioned Double)
getDouble expr = case expr of
    MValue v -> Just $ float2Double <$> v
    _ -> Nothing

getTime :: MondoExpr -> Maybe (Positioned T.Time)
getTime expr = case expr of
    MValue v -> Just $ toRational <$> v
    _ -> Nothing

getString :: MondoExpr -> Maybe (Positioned String)
getString expr = case expr of
    MPlain s -> Just s
    _ -> Nothing

resolve_seq :: (T.Parseable a, T.Enumerable a) => String -> (T.Pattern a -> T.ControlPattern) -> (MondoExpr -> Maybe (Positioned a)) -> MondoExpr -> Either ParseError T.ControlPattern
resolve_seq com app get expr = case expr of
    -- `s bd (hh # lpf 42)` is desugared to `(s bd (lpf 50 sd))`, and here,
    -- when we process `(lpf 50 sd)`, the following case rewrite it as: (lpf 50 (s sd))
    MList xs@(MPlain _ : _) ->
        let (i, l) = (init xs, last xs)
         in eval_list $ i <> [MList [MPlain (Positioned com 0 0), l]]
    _ -> app <$> eval_pat get expr

patWithPos :: Positioned a -> T.Pattern a
patWithPos v = (T.withContext (addPos v) $ pure v.value)

addPos :: Positioned a -> T.Context -> T.Context
addPos vp c = c{T.contextPosition = [((vp.col, vp.row), (vp.col + 1, vp.row))]}

mkError :: String -> P.SourcePos -> Either ParseError a
mkError s = Left . P.newErrorMessage (P.Message s)

exprPos :: MondoExpr -> P.SourcePos
exprPos expr = case expr of
    MPlain v -> posPos v
    MValue v -> posPos v
    MString v -> posPos v
    _ -> (P.newPos "input" 1 1)
  where
    posPos v = P.newPos "input" v.row v.col
