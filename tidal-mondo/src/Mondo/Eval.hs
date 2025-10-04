{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module Mondo.Eval where

import Data.List.NonEmpty qualified as NE
import GHC.Float (float2Double)

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

import Mondo.Parser
import Mondo.Token

eval :: MondoExpr -> Either ParseError T.ControlPattern
eval (MList xs) = eval_list newEnv xs
eval v = mkError ("expected a list, got: " <> show v) (P.newPos "input" 1 1)

pattern Com :: String -> MondoExpr
pattern Com s <- MPlain (Pos s)

data TidalPat a
    = Before (T.Pattern a -> T.ControlPattern -> T.ControlPattern)
    | After (T.Pattern a -> T.ControlPattern)

data Env = Env {envScale :: Maybe (T.Pattern Int -> T.ControlPattern)}

newEnv :: Env
newEnv = Env Nothing

eval_list :: Env -> [MondoExpr] -> Either ParseError T.ControlPattern
eval_list env es = case es of
    -- s is expected to be at the begining of a pattern
    (Com "s" : rest) -> T.fastCat <$> traverse (eval_last "s" T.sound getString) rest
    (Com "n" : rest)
        | -- note after sound, e.g. 's sine # n c2'
          Just (params, MList sound@(Com "s" : _)) <- unsnoc rest -> do
            soundControl <- eval_list env sound
            noteControl <- eval_notes params
            pure $ soundControl |+| noteControl
        | -- standalone n, without sound, e.g. for midi. This is also expected to be at the begining of a pattern
          otherwise ->
            eval_notes rest
    (Com "lpf" : rest) -> eval_control getDouble (After T.cutoff) rest
    (Com "fast" : rest) -> eval_control getTime (Before T.fast) rest
    (Com "slow" : rest) -> eval_control getTime (Before T.slow) rest
    (Com "scale" : rest)
        | -- scale pipe, e.g. 'n 0 # scale minor'
          Just (params, MList xs) <- unsnoc rest -> do
            scalePat <- eval_pats getString params
            -- add the scale to the env, and apply it later when encountering notes.
            eval_list (env{envScale = Just (T.scale scalePat)}) xs
    (MCommand "stack" : rest) -> T.stack <$> traverse eval rest
    x : _ -> mkError ("unexpected command: " <> show es) (exprPos x)
    [] -> mkError "expected command!" (P.newPos "input" 1 1)
  where
    eval_notes xs = case env.envScale of
        -- No scale defined, eval notes from pattern
        Nothing -> T.fastCat <$> traverse (eval_last "n" T.n getNote) xs
        -- Scale was piped, eval int from pattern
        Just scale -> T.fastCat <$> traverse (eval_last "n" scale getInt) xs
    eval_control get app xs
        | Just (params, MList rest) <- unsnoc xs = do
            paramPat <- eval_pats get params
            controlPat <- eval_list env rest
            pure $ case app of
                After aapp -> controlPat # aapp paramPat
                Before bapp -> bapp paramPat controlPat
        | otherwise = mkError ("invalid control pattern: " <> show xs) (exprPos $ MList xs)

-- | Evaluate a list of pattern like 'bd <sd hh>'
eval_pats :: (T.Parseable a, T.Enumerable a) => (MondoExpr -> Maybe (T.Pattern a)) -> [MondoExpr] -> Either ParseError (T.Pattern a)
eval_pats get xs = T.fastCat <$> traverse (eval_pat get) xs

-- | Evaluate a pattern like '<a b>'
eval_pat :: (T.Parseable a, T.Enumerable a) => (MondoExpr -> Maybe (T.Pattern a)) -> MondoExpr -> Either ParseError (T.Pattern a)
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
        Just v -> pure v
        Nothing -> mkError ("unexpected pat: " <> show expr) (exprPos expr)
  where
    eval_op getp app param val = do
        paramPat <- eval_pat getp param
        valPat <- eval_pat get val
        pure $ app paramPat valPat

-- | Evaluate the last pipe (e.g. the first pattern), like 'n' or 's', which supports 'Chaining Functions Locally'.
eval_last :: (T.Parseable a, T.Enumerable a) => String -> (T.Pattern a -> T.ControlPattern) -> (MondoExpr -> Maybe (T.Pattern a)) -> MondoExpr -> Either ParseError T.ControlPattern
eval_last com app get expr = case expr of
    MList [MCommand ":", note, sound] -> do
        soundPat <- eval_pat get sound
        notePat <- eval_pat getFloat note
        pure $ app soundPat |+| T.pF "n" notePat
    MList (Com "scale" : rest)
        | com == "n"
        , Just (params, MList note) <- unsnoc rest -> do
            notePat <- eval_pats getInt note
            scalePat <- eval_pats getString params
            pure $ T.scale scalePat notePat
    -- `s bd (hh # lpf 42)` is desugared to `(s bd (lpf 50 sd))`, and here,
    -- when we process `(lpf 50 sd)`, the following case rewrite it as: (lpf 50 (s sd))
    MList xs@(MPlain _ : _) ->
        let (i, l) = (init xs, last xs)
         in eval_list newEnv $ i <> [MList [MPlain (Positioned com 0 0), l]]
    _ -> app <$> eval_pat get expr

getDouble :: MondoExpr -> Maybe (T.Pattern Double)
getDouble expr = case expr of
    MValue v -> Just . patWithPos $ float2Double <$> v
    _ -> Nothing

getTime :: MondoExpr -> Maybe (T.Pattern T.Time)
getTime expr = case expr of
    MValue v -> Just . patWithPos $ toRational <$> v
    _ -> Nothing

getInt :: MondoExpr -> Maybe (T.Pattern Int)
getInt expr = case expr of
    MValue v -> Just . patWithPos $ round <$> v
    _ -> Nothing

getString :: MondoExpr -> Maybe (T.Pattern String)
getString expr = case expr of
    MPlain s -> Just . patWithPos $ s
    _ -> Nothing

getFloat :: MondoExpr -> Maybe (T.Pattern Double)
getFloat expr = case expr of
    MValue v -> Just . patWithPos $ float2Double <$> v
    _ -> Nothing

getNote :: MondoExpr -> Maybe (T.Pattern T.Note)
getNote expr = case expr of
    MPlain s -> case P.runParser T.pNote 0 "input" s.value of
        Left err -> error (show err)
        Right v -> Just (T.withContext (addPos s) $ T.toPat v)
    MValue v -> Just . patWithPos $ T.Note . float2Double <$> v
    _ -> Nothing

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
    MList (v : _) -> exprPos v
    _ -> (P.newPos "input" 1 1)
  where
    posPos v = P.newPos "input" v.row v.col

unsnoc :: [a] -> Maybe ([a], a)
unsnoc xs = do
    ne <- NE.nonEmpty xs
    pure (NE.init ne, NE.last ne)
