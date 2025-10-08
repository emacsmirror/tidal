{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module Mondo.Eval (eval) where

import Control.Monad (replicateM)
import Sound.Tidal.Control qualified as T
import Sound.Tidal.Core ((#), (|+), (|+|), (|-))
import Sound.Tidal.Core qualified as T
import Sound.Tidal.Params qualified as T
import Sound.Tidal.ParseBP qualified as T
import Sound.Tidal.Pattern qualified as T
import Sound.Tidal.Scales qualified as T
import Sound.Tidal.UI qualified as T
import Text.Parsec (ParseError)
import Text.Parsec qualified as P
import Text.Parsec.Error qualified as P
import Text.Parsec.Pos qualified as P

import Mondo.Params
import Mondo.Parser
import Mondo.Token

pattern Com :: String -> MondoExpr
pattern Com s <- MPlain (Pos s)

eval :: MondoExpr -> Either ParseError T.ControlPattern
eval es = case es of
    MList (MCommand "stack" : rest) ->
        let (defs, pats) = go [] [] rest
         in eval_list (Env Nothing Nothing defs) $ MCommand "stack" : pats
    _ -> eval_top newEnv es
  where
    go defs acc [] = (defs, reverse acc)
    go defs acc (x : xs) = case x of
        MList [Com "def", Com k, v] -> go ((k, v) : defs) acc xs
        _ -> go defs (x : acc) xs

eval_top :: Env -> MondoExpr -> Either ParseError T.ControlPattern
eval_top env (MList xs) = eval_list env xs
eval_top _ v = mkError ("expected a list, got: " <> show v) (P.newPos "input" 1 1)

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
    Com "add" : MList param : MList rest : [] -> do
        restPat <- eval_list env rest
        addPat <- eval_list env param
        pure $ restPat |+ addPat
    Com "sub" : MList param : MList rest : [] -> do
        restPat <- eval_list env rest
        subPat <- eval_list env param
        pure $ restPat |- subPat
    Com "rev" : MList rest : [] -> T.rev <$> eval_list env rest
    Com "dec" : rest@(_ : _) -> eval_control decPat rest
    Com "lpf" : rest@(_ : _) -> eval_control lpfPat rest
    Com "hpf" : rest@(_ : _) -> eval_control hpfPat rest
    Com "pan" : rest@(_ : _) -> eval_control panPat rest
    Com "fast" : rest@(_ : _) -> eval_mod fastPat rest
    Com "slow" : rest@(_ : _) -> eval_mod slowPat rest
    Com "iter" : rest@(_ : _) -> eval_mod iterPat rest
    Com "mask" : rest@(_ : _) -> eval_mod maskPat rest
    Com "euclid" : nparam : rest@(_ : _) -> do
        npat <- eval_ppat (mkMondoPat getInt) nparam
        eval_mod (euclidPat npat) rest
    Com "splice" : bitparam : rest@(_ : _) -> do
        bitpat <- eval_ppat (mkMondoPat getInt) bitparam
        eval_mod (splicePat bitpat) rest
    Com "jux" : param : MList rest : [] -> do
        f <- eval_fun env param
        restPat <- eval_list env rest
        pure $ T.jux f restPat
    Com "sometimes" : param : MList rest : [] -> do
        f <- eval_fun env param
        restPat <- eval_list env rest
        pure $ T.sometimes f restPat
    Com "scale" : param : MList rest : [] -> eval_scale param rest
    MCommand "n-colon-pat" : rest -> eval_control nColonPat rest
    MCommand "stack" : rest -> T.stack <$> traverse (eval_top env) rest
    x : _ -> mkError ("unexpected command: " <> show es) (exprPos x)
    [] -> mkError "expected command!" (P.newPos "input" 1 1)
  where
    eval_ppat mpat expr = snd <$> eval_pat env mpat expr

    eval_notes param = case env.envScale of
        -- No scale defined, eval notes from pattern
        Nothing -> eval_ppat nPat param
        -- Scale was piped, eval int from pattern
        Just scale -> eval_ppat (mkScalePat scale) param

    eval_scale param rest = do
        scalePat <- eval_ppat (mkMondoPat getString) param
        case eval_pat env (mkMondoPat getInt) (MList rest) of
            -- rest are notes, apply the scale and make a control param
            Right (_, notePat) -> pure $ T.scale scalePat notePat
            -- rest is probably a pipe, add the scale to the env, and apply it later when encountering notes.
            Left _ -> eval_list (env{envScale = Just (T.scale scalePat)}) rest

    -- Evaluate a control pattern like 's bd'
    eval_control _ [] = error "The impossible has happened!"
    eval_control mondoPat (param : rest) = do
        paramPat <- eval_ppat mondoPat param
        case rest of
            [MList xs] -> do
                restPat <- eval_list env xs
                pure $ mondoPat.combiner restPat paramPat
            [] -> pure paramPat
            -- Lambda variable can be ignored?!
            [MCommand "_"] -> pure paramPat
            -- Here we don't know what's the command, see Note [Chaining Functions Locally]
            [v@(MPlain _)] | Just currentParam <- env.currentParam -> do
                restPat <- eval_list env [currentParam, v]
                pure $ mondoPat.combiner paramPat restPat
            [v@(MValue _)] | Just currentParam <- env.currentParam -> do
                restPat <- eval_list env [currentParam, v]
                pure $ mondoPat.combiner paramPat restPat
            other -> mkError ("unexpected control: " <> show other) $ exprPos (MList other)

    -- Evaluate a modifier pattern like 'fast 2'
    eval_mod _ [] = error "The impossible has happened!"
    eval_mod mondoMod (param : rest) = do
        controlPat <- eval_ppat (mkMondoPat mondoMod.exprToPat) param
        restPat <- case rest of
            [MList xs] -> eval_list env xs
            _ -> mkError ("expected command, got: " <> show rest) (exprPos (MList rest))
        pure $ mondoMod.appModifier controlPat restPat

eval_fun :: Env -> MondoExpr -> Either ParseError (T.ControlPattern -> T.ControlPattern)
eval_fun env expr = case expr of
    MList [MLam _ body] -> eval_fun env body
    MList (Com "jux" : x : rest) -> do
        f <- eval_fun env x
        eval_compo (T.jux f) rest
    MCommand "_" -> pure id
    Com "rev" -> pure T.rev
    MList xs -> case eval_list env xs of
        Left _ -> mkError ("arg is not fun: " <> show expr) (exprPos expr)
        Right p -> pure (# p)
    _ -> mkError ("expected fun, got: " <> show expr) (exprPos expr)
  where
    eval_compo f rest = case rest of
        [] -> pure f
        [x] -> do
            g <- eval_fun env x
            pure $ f . g
        _ -> mkError ("unexpected fun: " <> show rest) (exprPos (MList rest))

-- Evaluate a 'MondoExpr', according to a 'MondoPat', into a tidal pattern.
eval_pat :: (T.Parseable a, T.Enumerable a) => Env -> MondoPat a b -> MondoExpr -> Either ParseError (Rational, T.Pattern b)
eval_pat env mpat expr = case expr of
    -- Use tidal mini notation
    MString p -> case T.parseBP p.value of
        Left err ->
            let errPos = P.errorPos err
                newPos = P.incSourceLine (P.incSourceColumn errPos p.col) p.row
             in Left $ P.setErrorPos newPos err
        Right pat -> pure (1, T.withContext (addPos p) (mpat.patToControl pat))
    -- < >
    MList (MCommand "angle" : xs) ->
        (1,) <$> T.slow (pure $ toRational $ length xs) . T.timecat <$> traverse (eval_pat env mpat) xs
    -- [ ]
    MList (MCommand "square" : xs) -> (1,) <$> T.timecat <$> traverse (eval_pat env mpat) xs
    -- x?
    MList [MCommand "?", x] -> fmap (T.degradeBy 0.5) <$> eval_pat env mpat x
    -- x?y
    MList [MCommand "?", x, y] -> do
        yPat <- snd <$> eval_pat env (mkMondoPat getDouble) y
        fmap (T.degradeBy yPat) <$> eval_pat env mpat x
    -- x*y
    MList [MCommand "*", param, val] -> eval_op getTime T.fast param val
    MList [Com "fast", param, val] -> eval_op getTime T.fast param val
    -- x/y
    MList [MCommand "/", param, val] -> eval_op getTime T.slow param val
    MList [Com "slow", param, val] -> eval_op getTime T.slow param val
    -- range x y p
    MList [Com "range", x, y, p]
        | Just rangeOp <- mpat.rangeOp -> do
            let rpat = mkMondoPat getDouble
            xPat <- snd <$> eval_pat env rpat x
            yPat <- snd <$> eval_pat env rpat y
            (l, pPat) <- eval_pat env rpat p
            pure (l, mpat.patToControl $ rangeOp xPat yPat pPat)
    -- x..y
    MList [MCommand "..", y, x] -> do
        let rpat = mkMondoPat mpat.exprToPat
        xPat <- snd <$> eval_pat env rpat x
        yPat <- snd <$> eval_pat env rpat y
        pure (1, mpat.patToControl $ T.unwrap $ T.fromTo <$> xPat <*> yPat)
    -- x:y
    MList [MCommand ":", note, sound]
        | Just (Com "s") <- mpat.localExpr
        , Just colonOp <- mpat.colonOp -> do
            (l, soundPat) <- eval_pat env mpat sound
            notePat <- snd <$> eval_pat env colonSoundPat note
            pure (l, colonOp soundPat notePat)
    MList [MCommand ":", z, y]
        | Just (MCommand "&") <- mpat.localExpr
        , Just andOp <- mpat.andOp -> do
            yPat <- snd <$> eval_pat env (mkMondoPat getInt) y
            zPat <- snd <$> eval_pat env (mkMondoPat getInt) z
            pure (1, andOp yPat zPat)
    -- x!y
    MList [MCommand "!", MValue (Pos y), x] -> do
        (fromInteger $ round y,) <$> T.timecat <$> replicateM (round y) (eval_pat env mpat x)
    -- x@y
    -- Note: y can't be a pattern, like `[bd@<2 6> sd]` parses in both tidal and strudel, but it doesn't do what you would expect.
    -- Good, because that would be hard to support here:) so `y` must be a value
    MList [MCommand "@", MValue (Pos y), x] -> do
        p <- snd <$> eval_pat env mpat x
        pure (fromInteger $ round y, p)
    -- x&y:z
    MList [MCommand "&", x, xs] -> do
        (l, xPat) <- eval_pat env mpat x
        let epat = mpat{localExpr = Just (MCommand "&"), andOp = Just (\yPat zPat -> T.euclid yPat zPat xPat)}
        (l,) <$> T.timecat <$> traverse (eval_pat env epat) [xs]
    -- ~
    Com "~" -> pure (1, T.silence)
    -- see Note [Chaining Functions Locally]
    MList xs | Just nested <- mpat.nested -> (1,) <$> nested (env{currentParam = mpat.localExpr}) xs
    -- a def
    Com n | Just v <- lookup n env.defs -> eval_pat env mpat v
    -- this is a value, make it a pattern.
    _ | Just v <- mpat.exprToPat expr -> pure $ (1, mpat.patToControl v)
    _ -> mkError ("unexpected pat: " <> show expr) (exprPos expr)
  where
    eval_op getp appOp param val = do
        paramPat <- snd <$> eval_pat env (mkMondoPat getp) param
        (l, valPat) <- eval_pat env mpat val
        pure (l, appOp paramPat valPat)

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
applied when a param argument is plain.
-}

mkP :: String -> MondoExpr
mkP n = MPlain (Positioned n 0 0)

-- * Helpers to map mondo to tidal
mkMondoParam :: String -> (MondoExpr -> Maybe (T.Pattern a)) -> (T.Pattern a -> T.ControlPattern) -> MondoParam a
mkMondoParam name get app =
    MondoPat
        { localExpr = Just $ mkP name
        , exprToPat = get
        , patToControl = app
        , colonOp = Just (|+|)
        , andOp = Nothing
        , rangeOp = Nothing
        , combiner = (#)
        , nested = Just eval_list
        }

mkMondoDParam :: String -> (MondoExpr -> Maybe (T.Pattern Double)) -> (T.Pattern Double -> T.ControlPattern) -> MondoParam Double
mkMondoDParam name get app = (mkMondoParam name get app){rangeOp = Just T.range}

-- * Control Patterns

sPat :: MondoParam String
sPat = (mkMondoParam "s" getString T.sound){combiner = (flip (#))}

nPat :: MondoParam T.Note
nPat = (mkMondoParam "n" getNote T.n){combiner = (|+|)}

lpfPat :: MondoParam Double
lpfPat = mkMondoDParam "lpf" getDouble T.cutoff

hpfPat :: MondoParam Double
hpfPat = mkMondoDParam "hpf" getDouble T.hcutoff

panPat :: MondoParam Double
panPat = mkMondoDParam "pan" getDouble T.pan

decPat :: MondoParam Double
decPat = mkMondoDParam "dec" getDouble T.decay

mkScalePat :: (T.Pattern Int -> T.ControlPattern) -> MondoParam Int
mkScalePat scale = (mkMondoParam "scale" getInt scale){combiner = (|+|)}

-- * Grp Patterns

nColonPat :: MondoParam Double
nColonPat = MondoPat Nothing getDouble (T.pF "n") Nothing Nothing Nothing const Nothing

colonSoundPat :: MondoParam Double
colonSoundPat = (mkMondoParam "" getDouble (T.pF "n")){localExpr = Just $ MCommand "n-colon-pat"}

-- * Modifier Patterns

fastPat, slowPat :: MondoMod T.Time
fastPat = MondoMod getTime T.fast
slowPat = MondoMod getTime T.slow

splicePat :: T.Pattern Int -> MondoMod Int
splicePat bitpat = MondoMod getInt (T.splice bitpat)

euclidPat :: T.Pattern Int -> MondoMod Int
euclidPat n = MondoMod getInt (T.euclid n)

iterPat :: MondoMod Int
iterPat = MondoMod getInt T.iter

maskPat :: MondoMod Bool
maskPat = MondoMod getBool T.mask
