{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Mondo.Params where

import GHC.Float (float2Double)
import Text.Parsec qualified as P

import Sound.Tidal.Core qualified as T
import Sound.Tidal.ParseBP qualified as T
import Sound.Tidal.Pattern qualified as T

import Mondo.Parser
import Mondo.Token (Positioned (..))

-- | The 'Env' keeps track of attributes that can be set through pipes
data Env = Env
    { envScale :: Maybe (T.Pattern Int -> T.ControlPattern)
    -- ^ A scale set like this "n 0 # scale minor", it will be applied when encountering a note pattern.
    , currentParam :: Maybe MondoExpr
    -- ^ The current param, see Note [Depend on Chaining Functions Locally]
    , defs :: [(String, MondoExpr)]
    }

newEnv :: Env
newEnv = Env Nothing Nothing []

-- | A pattern that can be parsed with 'eval_pat'.
data MondoPat a b = MondoPat
    { localExpr :: Maybe MondoExpr
    -- ^ The local expr, use to decide how to handle ':' operation
    , exprToPat :: MondoExpr -> Maybe (T.Pattern a)
    -- ^ How to read a MondoExpr, e.g. getString.
    , patToControl :: T.Pattern a -> T.Pattern b
    -- ^ How to make a ControlPattern.
    , colonOp :: Maybe (T.Pattern b -> T.ControlPattern -> T.Pattern b)
    -- ^ How to handle the ':' operator.
    , andOp :: Maybe (T.Pattern Int -> T.Pattern Int -> T.Pattern b)
    -- ^ How to handle the '&' operator.
    , rangeOp :: Maybe (T.Pattern Double -> T.Pattern Double -> T.Pattern Double -> T.Pattern a)
    , combiner :: T.ControlPattern -> T.ControlPattern -> T.ControlPattern
    -- ^ How to combine the resulting pattern with the remaining pipes.
    , nested :: Maybe (Env -> [MondoExpr] -> Either P.ParseError (T.Pattern b))
    -- ^ How to evaluated nested expression, see Note [Chaining Functions Locally]
    }

-- | Create the simplest pattern, useful for example to parse the notes from 'bd:<1 2>'
mkMondoPat :: (MondoExpr -> Maybe (T.Pattern a)) -> MondoPat a a
mkMondoPat exprToPat = MondoPat Nothing exprToPat id Nothing Nothing Nothing const Nothing

type MondoParam a = MondoPat a T.ValueMap

-- | A modifier pattern, like for 'fast' or 'slow'.
data MondoMod a = MondoMod
    { exprToPat :: MondoExpr -> Maybe (T.Pattern a)
    , appModifier :: T.Pattern a -> T.ControlPattern -> T.ControlPattern
    }

getDouble :: MondoExpr -> Maybe (T.Pattern Double)
getDouble expr = case expr of
    MValue v -> Just . patWithPos $ float2Double <$> v
    MPlain (Positioned "sine" _ _ _) -> Just $ T.sine
    _ -> Nothing

getTime :: MondoExpr -> Maybe (T.Pattern T.Time)
getTime expr = case expr of
    MValue v -> Just . patWithPos $ toRational <$> v
    _ -> Nothing

getInt :: MondoExpr -> Maybe (T.Pattern Int)
getInt expr = case expr of
    MValue v -> Just . patWithPos $ round <$> v
    _ -> Nothing

getBool :: MondoExpr -> Maybe (T.Pattern Bool)
getBool expr = case expr of
    MValue v -> Just . patWithPos $ const (v.value /= 0) <$> v
    _ -> Nothing

getString :: MondoExpr -> Maybe (T.Pattern String)
getString expr = case expr of
    MPlain s -> Just . patWithPos $ s
    _ -> Nothing

getNote :: MondoExpr -> Maybe (T.Pattern T.Note)
getNote expr = case expr of
    MPlain s -> case P.runParser T.pNote 0 "input" s.value of
        Left err -> error (show err)
        Right v -> Just (T.withContext (addPos s) $ T.toPat v)
    MValue v -> Just . patWithPos $ T.Note . float2Double <$> v
    _ -> Nothing

patWithPos :: Positioned a -> T.Pattern a
patWithPos v = T.withContext (addPos v) $ pure v.value

addPos :: Positioned a -> T.Context -> T.Context
addPos vp c = c{T.contextPosition = [((vp.col, vp.row), (vp.col + vp.len, vp.row))]}
