{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

module Mondo.Params where

import Data.Map.Strict qualified as Map
import GHC.Float (float2Double)
import Text.Parsec qualified as P

import Sound.Tidal.Core qualified as T
import Sound.Tidal.ParseBP qualified as T
import Sound.Tidal.Pattern qualified as T
import Sound.Tidal.UI qualified as T

import Mondo.Parser
import Mondo.Tidal
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
    , rangenOp :: Maybe (T.Pattern T.Note -> T.Pattern T.Note -> T.Pattern T.Note -> T.Pattern a)
    -- ^ How to handle the range operator.
    , fromNote :: Maybe (T.Pattern T.Note -> T.Pattern b)
    -- ^ How to convert from note patterns like irand
    , fromInt :: Maybe (T.Pattern Int -> T.Pattern b)
    -- ^ How to convert from int patterns like randrun
    , fromControl :: Maybe (T.ControlPattern -> T.Pattern b)
    -- ^ How to evaluated nested expression, see Note [Chaining Functions Locally]
    }

{-
Note [Chaining Functions Locally]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Supporting the feature described in https://strudel.cc/learn/mondo-notation/#chaining-functions-locally
requires a work-around:
The mondo expression: 's [bd hh bd (cp # delay .6)] # bank tr909'
desugar into: '(bank tr909 (s (square bd hh bd (delay .6 cp))))'

When evaluating the very last expression: '(delay .6 cp)', we don't know what 'cp' is.
Thus, the parent control pattern is passed as the 'currentParam' environment, and it is
applied when a param argument is plain.
-}

mkP :: String -> MondoExpr
mkP n = MPlain (Positioned n 0 0 0)

-- * Helpers to map mondo to tidal
mkMondoParam :: String -> (MondoExpr -> Maybe (T.Pattern a)) -> (T.Pattern a -> T.ControlPattern) -> MondoParam a
mkMondoParam name get app =
    MondoPat
        { localExpr = Just $ mkP name
        , exprToPat = get
        , patToControl = app
        , colonOp = Just (T.|+|)
        , andOp = Nothing
        , rangeOp = Nothing
        , rangenOp = Nothing
        , fromControl = Just id
        , fromNote = Nothing
        , fromInt = Nothing
        }

mkMondoNParam :: String -> (MondoExpr -> Maybe (T.Pattern T.Note)) -> (T.Pattern T.Note -> T.ControlPattern) -> MondoParam T.Note
mkMondoNParam name get app = (mkMondoParam name get app){rangenOp = Just T.range, fromNote = Just app, fromInt = Just (app . fmap fromIntegral)}

mkMondoDParam :: String -> (T.Pattern Double -> T.ControlPattern) -> MondoParam Double
mkMondoDParam name app = (mkMondoParam name getDouble app){rangeOp = Just T.range}

-- | Create the simplest pattern, useful for example to parse the notes from 'bd:<1 2>'
mkMondoPat :: (MondoExpr -> Maybe (T.Pattern a)) -> MondoPat a a
mkMondoPat exprToPat = MondoPat Nothing exprToPat id Nothing Nothing Nothing Nothing Nothing Nothing Nothing

type MondoParam a = MondoPat a T.ValueMap

getDouble :: MondoExpr -> Maybe (T.Pattern Double)
getDouble expr = case expr of
    MValue v -> Just . patWithPos $ float2Double <$> v
    MPlain (Positioned n _ _ _)
        | Just p <- Map.lookup n pFrac -> Just $ p
        | Just p <- Map.lookup n pFracReal -> Just $ p
    _ -> Nothing

getTime :: MondoExpr -> Maybe (T.Pattern T.Time)
getTime expr = case expr of
    MValue v -> Just . patWithPos $ toRational <$> v
    MPlain (Positioned n _ _ _)
        | Just p <- Map.lookup n pFrac -> Just $ toRational @Double <$> p
        | Just p <- Map.lookup n pFracReal -> Just $ toRational @Double <$> p
    _ -> Nothing

getInt :: MondoExpr -> Maybe (T.Pattern Int)
getInt expr = case expr of
    MValue v -> Just . patWithPos $ round <$> v
    MPlain (Positioned n _ _ _)
        | Just p <- Map.lookup n pFrac -> Just $ round @Double <$> p
        | Just p <- Map.lookup n pFracReal -> Just $ round @Double <$> p
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
    MPlain (Positioned n _ _ _)
        | Just p <- Map.lookup n pFrac -> Just $ T.Note <$> p
    MPlain s -> case P.runParser T.pNote 0 "input" s.value of
        Left err -> error (show err)
        Right v -> Just (T.withContext (addPos s) $ T.toPat v)
    MValue v -> Just . patWithPos $ T.Note . float2Double <$> v
    _ -> Nothing

patWithPos :: Positioned a -> T.Pattern a
patWithPos v = T.withContext (addPos v) $ pure v.value

addPos :: Positioned a -> T.Context -> T.Context
addPos vp c = c{T.contextPosition = [((max 0 $ vp.col - 1, vp.row), (max 1 $ vp.col + vp.len - 1, vp.row))]}
