{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module Mondo.Eval (eval) where

import Control.Monad (replicateM)
import Data.Map.Strict qualified as Map
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
import Mondo.Tidal
import Mondo.Token

pattern Com :: String -> MondoExpr
pattern Com s <- MPlain (Pos s)

eval :: MondoExpr -> Either ParseError T.ControlPattern
eval es = case (eval_maths es) of
    MList (MCommand "stack" : rest) ->
        let (defs, pats) = go [] [] rest
         in eval_list (Env Nothing Nothing defs) $ MCommand "stack" : pats
    pats -> eval_top newEnv pats
  where
    go defs acc [] = (defs, reverse acc)
    go defs acc (x : xs) = case x of
        MList [Com "def", Com k, v] -> go ((k, v) : defs) acc xs
        MList rest | isMuted rest -> go defs acc xs
        _ -> go defs (x : acc) xs
    isMuted [] = False
    isMuted (Com "_" : _) = True
    isMuted xs = case last xs of
        MList rest -> isMuted rest
        _ -> False

mathOp :: String -> Maybe (Float -> Float -> Float)
mathOp "/" = Just (/)
mathOp "+" = Just (+)
mathOp "-" = Just (-)
mathOp _ = Nothing

eval_maths :: MondoExpr -> MondoExpr
eval_maths expr = case expr of
    MList [MCommand mathCommand, y, x] -- note: ops arg are inverted
        | MValue xv <- eval_maths x
        , MValue yv <- eval_maths y
        , Just op <- mathOp mathCommand ->
            let v = (op xv.value yv.value)
                l = yv.col - xv.col + yv.len
             in MValue (Positioned v yv.col yv.row l)
    MList xs -> MList $ map eval_maths xs
    MLam x y -> MLam x (eval_maths y)
    other -> other

eval_top :: Env -> MondoExpr -> Either ParseError T.ControlPattern
eval_top env (MList xs) = eval_list env xs
eval_top _ v = mkError ("expected a list, got: " <> show v) (P.newPos "input" 1 1)

eval_list :: Env -> [MondoExpr] -> Either ParseError T.ControlPattern
eval_list env es = case es of
    -- When notes are at the end, just eval the pattern
    Com "n" : param : [] -> eval_notes param
    -- When notes are in the middle, eval the rest first then apply the notes with |+|
    Com "n" : param : MList rest : [] -> do
        restPat <- eval_list env rest
        notePat <- eval_notes param
        pure $ restPat |+| notePat
    -- When notes are in the middle, eval the rest first then apply the notes with |+|
    Com "note" : param : MList rest : [] -> do
        restPat <- eval_list env rest
        notePat <- eval_ppat (mkMondoNParam "note" getNote T.note) param
        pure $ restPat |+| notePat
    -- add/sub are custom mondo functions to control how the pattern are applied to the chain
    Com "add" : MList param : MList rest : [] -> do
        restPat <- eval_list env rest
        addPat <- eval_list env param
        pure $ restPat |+ addPat
    Com "sub" : MList param : MList rest : [] -> do
        restPat <- eval_list env rest
        subPat <- eval_list env param
        pure $ restPat |- subPat
    -- ControlPatterns like 'sound'
    Com n : param : rest
        | Just f <- Map.lookup n pStr_pC -> eval_control (mkMondoParam n getString f) param rest
        | Just f <- Map.lookup n pDouble_pC -> eval_control (mkMondoDParam n f) param rest
        | Just f <- Map.lookup n pInt_pC -> eval_control (mkMondoParam n getInt f) param rest
        | Just f <- Map.lookup n pNote_pC -> eval_control (mkMondoParam n getNote f) param rest
    -- Generic p* control patterns
    Com p : Com name : param : rest
        | p == "pF" -> eval_control (mkMondoParam name getDouble (T.pF name)) param rest
        | p == "pI" -> eval_control (mkMondoParam name getInt (T.pI name)) param rest
        | p == "pN" -> eval_control (mkMondoParam name getNote (T.pN name)) param rest
        | p == "pS" -> eval_control (mkMondoParam name getString (T.pS name)) param rest
        | p == "pR" -> eval_control (mkMondoParam name getTime (T.pR name)) param rest
        | p == "pB" -> eval_control (mkMondoParam name getBool (T.pB name)) param rest
    -- Direct modifiers like 'rev'
    Com n : MList rest : []
        | Just f <- Map.lookup n pA_pA -> f <$> eval_list env rest
        | Just f <- Map.lookup n pC_pC -> f <$> eval_list env rest
    -- Modifier with literal param
    Com n : MValue v : MList rest : []
        | Just f <- Map.lookup n time_pC_pC -> f (toRational v.value) <$> eval_list env rest
    Com n : MValue x : MValue y : MList rest : []
        | Just f <- Map.lookup n int_time_pA_pA -> do
            f (round x.value) (toRational y.value) <$> eval_list env rest
    -- Modifier with 2 pattern params
    Com n : param1 : param2 : MList rest : []
        | Just f <- Map.lookup n pTime_pTime_pC_pC -> eval_mod2 f getTime getTime param1 param2 rest
        | Just f <- Map.lookup n pInt_pInt_pC_pC -> eval_mod2 f getInt getInt param1 param2 rest
        | Just f <- Map.lookup n pInt_pDouble_pC_pC -> eval_mod2 f getInt getDouble param1 param2 rest
    -- Modifier with 1 pattern param
    Com n : param : MList rest : []
        | Just f <- Map.lookup n pTime_pA_pA -> eval_mod getTime f param rest
        | Just f <- Map.lookup n pTime_pC_pC -> eval_mod getTime f param rest
        | Just f <- Map.lookup n pBool_pA_pA -> eval_mod getBool f param rest
        | Just f <- Map.lookup n pInt_pA_pA -> eval_mod getInt f param rest
        | Just f <- Map.lookup n pInt_pC_pC -> eval_mod getInt f param rest
        | Just f <- Map.lookup n pS_pA_pA -> eval_mod getString f param rest
        | Just f <- Map.lookup n pInt_pOrd_pOrd -> eval_mod getInt f param rest
        | Just f <- Map.lookup n pCpC_pC_pC -> eval_fmod f param rest
        | Just f <- Map.lookup n pApA_pA_pA -> eval_fmod f param rest
    Com n : param1 : param2 : MList rest : []
        | Just f <- Map.lookup n pInt_pApA_pA_pA -> do
            npat <- eval_ppat (mkMondoPat getInt) param1
            fa <- eval_fun env param2
            f npat fa <$> eval_list env rest
        | Just f <- Map.lookup n pTime_pApA_pA_pA -> do
            npat <- eval_ppat (mkMondoPat getTime) param1
            fa <- eval_fun env param2
            f npat fa <$> eval_list env rest
    Com n : param1 : param2 : param3 : MList rest : []
        | Just f <- Map.lookup n pInt_pTime_pDouble_pC_pC -> do
            pat1 <- eval_ppat (mkMondoPat getInt) param1
            pat2 <- eval_ppat (mkMondoPat getTime) param2
            eval_mod getDouble (f pat1 pat2) param3 rest
    -- scale is custom in mondo so that it can be used after the notes like 'n 0 # scale minor'
    Com "scale" : param : MList rest : [] -> eval_scale param rest
    -- n-colon-pat is injected by the eval_pat, when evaluating expression like 'bd:<(1 # lpf 42)>'
    MCommand "n-colon-pat" : param : rest -> eval_control (mkMondoParam "" getDouble (T.pF "n")) param rest
    -- stack separate mondo pattern, it is injected with '$'
    MCommand "stack" : rest -> T.stack <$> traverse (eval_top env) rest
    x : _ -> mkError ("unexpected command: " <> show es) (exprPos x)
    [] -> mkError "expected command!" (P.newPos "input" 1 1)
  where
    eval_ppat mpat expr = snd <$> eval_pat False env mpat expr

    eval_notes param = case env.envScale of
        -- No scale defined, eval notes from pattern
        Nothing -> eval_ppat (mkMondoNParam "n" getNote T.n) param
        -- Scale was piped, eval int from pattern
        Just scale -> eval_ppat (mkMondoParam "scale" getInt scale) param

    eval_scale param rest = do
        scalePat <- eval_ppat (mkMondoPat getString) param
        case eval_pat False env (mkMondoPat getInt) (MList rest) of
            -- rest are notes, apply the scale and make a control param
            Right (_, notePat) -> pure $ T.scale scalePat notePat
            -- rest is probably a pipe, add the scale to the env, and apply it later when encountering notes.
            Left _ -> eval_list (env{envScale = Just (T.scale scalePat)}) rest

    -- Evaluate a control pattern like 's bd'
    eval_control mondoPat param rest = do
        paramPat <- eval_ppat mondoPat param
        case rest of
            [MList xs] -> do
                restPat <- eval_list env xs
                pure $ restPat # paramPat
            [] -> pure paramPat
            -- Lambda variable can be ignored?!
            [MCommand "_"] -> pure paramPat
            -- Here we don't know what's the command, see Note [Chaining Functions Locally]
            [v@(MPlain _)] | Just currentParam <- env.currentParam -> do
                restPat <- eval_list env [currentParam, v]
                pure $ restPat # paramPat
            [v@(MValue _)] | Just currentParam <- env.currentParam -> do
                restPat <- eval_list env [currentParam, v]
                pure $ restPat # paramPat
            other -> mkError ("unexpected control: " <> show other) $ exprPos (MList other)

    eval_mod2 f get1 get2 param1 param2 rest = do
        npat1 <- eval_ppat (mkMondoPat get1) param1
        eval_mod get2 (f npat1) param2 rest
    -- Evaluate a modifier pattern like 'fast 2'
    eval_mod get app param rest = do
        controlPat <- eval_ppat (mkMondoPat get) param
        restPat <- eval_list env rest
        pure $ app controlPat restPat
    eval_fmod app param rest = do
        f <- eval_fun env param
        restPat <- eval_list env rest
        pure $ app f restPat

eval_fun :: Env -> MondoExpr -> Either ParseError (T.ControlPattern -> T.ControlPattern)
eval_fun env expr = case expr of
    MList [MLam _ body] -> eval_fun env body
    MList (Com "add" : MList rest : []) -> do
        p <- eval_list env rest
        pure (|+ p)
    MList (Com n : x : rest)
        | Just pf <- Map.lookup n pCpC_pC_pC -> do
            f <- eval_fun env x
            eval_compo (pf f) rest
        | Just f <- Map.lookup n pTime_pA_pA -> eval_mod getTime f x rest
        | Just f <- Map.lookup n pTime_pC_pC -> eval_mod getTime f x rest
        | Just f <- Map.lookup n pS_pA_pA -> eval_mod getString f x rest
        | Just f <- Map.lookup n pInt_pOrd_pOrd -> eval_mod getInt f x rest
        | Just f <- Map.lookup n pInt_pC_pC -> eval_mod getInt f x rest
    MCommand "_" -> pure id
    Com n
        | Just f <- Map.lookup n pA_pA -> pure f
        | Just f <- Map.lookup n pC_pC -> pure f
    MList xs -> case eval_list env xs of
        Left _ -> mkError ("arg is not fun: " <> show expr) (exprPos expr)
        Right p -> pure (# p)
    _ -> mkError ("expected fun, got: " <> show expr) (exprPos expr)
  where
    eval_mod get app param rest = do
        a <- snd <$> eval_pat False env (mkMondoPat get) param
        eval_compo (app a) rest
    eval_compo f rest = case rest of
        [] -> pure f
        [x] -> do
            g <- eval_fun env x
            pure $ f . g
        _ -> mkError ("unexpected fun: " <> show rest) (exprPos (MList rest))

-- Evaluate a 'MondoExpr', according to a 'MondoPat', into a tidal pattern.
eval_pat :: (T.Parseable a, T.Enumerable a, Ord a) => Bool -> Env -> MondoPat a b -> MondoExpr -> Either ParseError (Rational, T.Pattern b)
eval_pat highlight env mpat expr = case expr of
    -- Use tidal mini notation
    MString p -> case T.parseBP p.value of
        Left err ->
            let errPos = P.errorPos err
                newPos = P.incSourceLine (P.incSourceColumn errPos p.col) p.row
             in Left $ P.setErrorPos newPos err
        Right pat ->
            pure (1, T.deltaContext (p.col - 1) (p.row - 1) (mpat.patToControl pat))
    -- < >
    MList (MCommand "angle" : xs) ->
        (1,) <$> T.slow (pure $ toRational $ length xs) . T.timecat <$> traverse (eval_pat True env mpat) xs
    -- [ ]
    MList (MCommand "square" : xs) -> (1,) <$> T.timecat <$> traverse (eval_pat True env mpat) xs
    MList (MCommand "stack" : xs) -> (1,) . T.stack . map snd <$> traverse (eval_pat True env mpat) xs
    -- x?
    MList [MCommand "?", x] -> fmap (T.degradeBy 0.5) <$> eval_ppat mpat x
    -- x?y
    MList [MCommand "?", x, y] -> do
        yPat <- snd <$> eval_ppat (mkMondoPat getDouble) y
        fmap (T.degradeBy yPat) <$> eval_ppat mpat x
    -- x*y
    MList [MCommand "*", param, val] -> eval_op getTime T.fast param val
    -- x/y
    MList [MCommand "/", param, val] -> eval_op getTime T.slow param val
    -- range x y p
    MList [Com "range", x, y, p]
        | -- double range
          Just rangeOp <- mpat.rangeOp -> do
            let rpat = mkMondoPat getDouble
            xPat <- snd <$> eval_ppat rpat x
            yPat <- snd <$> eval_ppat rpat y
            (l, pPat) <- eval_ppat rpat p
            pure (l, mpat.patToControl $ rangeOp xPat yPat pPat)
        | -- note range
          Just rangeOp <- mpat.rangenOp -> do
            let rpat = mkMondoPat getNote
            xPat <- snd <$> eval_ppat rpat x
            yPat <- snd <$> eval_ppat rpat y
            (l, pPat) <- eval_ppat rpat p
            pure (l, mpat.patToControl $ rangeOp xPat yPat pPat)
    -- x..y
    MList [MCommand "..", y, x] -> do
        let rpat = mkMondoPat mpat.exprToPat
        xPat <- snd <$> eval_ppat rpat x
        yPat <- snd <$> eval_ppat rpat y
        pure (1, mpat.patToControl $ T.unwrap $ T.fromTo <$> xPat <*> yPat)
    -- x:y
    MList [MCommand ":", x, y]
        | isSound mpat.localExpr
        , Just colonOp <- mpat.colonOp -> do
            let colonSoundPat = (mkMondoParam "" getDouble (T.pF "n")){localExpr = Just $ MCommand "n-colon-pat"}
            (l, soundPat) <- eval_ppat mpat y
            notePat <- snd <$> eval_ppat colonSoundPat x
            pure (l, colonOp soundPat notePat)
        | Just (Com "vib") <- mpat.localExpr
        , Just colonOp <- mpat.colonOp -> do
            (l, vibPat) <- eval_ppat mpat y
            vibmodPat <- snd <$> eval_ppat (mkMondoParam "" getDouble (T.pF "vibmod")) x
            pure (l, colonOp vibPat vibmodPat)
        | Just (Com "distort") <- mpat.localExpr
        , Just colonOp <- mpat.colonOp -> do
            (l, yPat) <- eval_ppat mpat y
            xPat <- snd <$> eval_ppat (mkMondoParam "" getDouble (T.pF "distortvol")) x
            pure (l, colonOp yPat xPat)
        | Just (MCommand "&") <- mpat.localExpr
        , Just andOp <- mpat.andOp -> do
            yPat <- snd <$> eval_ppat (mkMondoPat getInt) y
            zPat <- snd <$> eval_ppat (mkMondoPat getInt) x
            pure (1, andOp yPat zPat)
    -- x!y
    MList [MCommand "!", MValue (Pos y), x] -> do
        (fromInteger $ round y,) <$> T.timecat <$> replicateM (round y) (eval_ppat mpat x)
    -- x@y
    -- Note: y can't be a pattern, like `[bd@<2 6> sd]` parses in both tidal and strudel, but it doesn't do what you would expect.
    -- Good, because that would be hard to support here:) so `y` must be a value
    MList [MCommand "@", MValue (Pos y), x] -> do
        p <- snd <$> eval_ppat mpat x
        pure (fromInteger $ round y, p)
    -- x&y:z euclidean, see: https://codeberg.org/uzu/strudel/pulls/1630
    MList [MCommand "&", x, xs] -> do
        (l, xPat) <- eval_ppat mpat x
        let epat = mpat{localExpr = Just (MCommand "&"), andOp = Just (\yPat zPat -> T.euclid yPat zPat xPat)}
        (l,) <$> T.timecat <$> traverse (eval_ppat epat) [xs]
    -- ~
    Com "~" -> pure (1, T.silence)
    -- Modifier with 1 pattern param, like fast or segment
    MList [Com n, arg, rest]
        | Just tmod <- Map.lookup n pTime_pA_pA -> eval_op getTime tmod arg rest
    MList [Com n, arg]
        | Just mk <- mpat.fromNote
        , Just f <- Map.lookup n pInt_pNum ->
            fmap (mk . f) <$> eval_ppat (mkMondoPat getInt) arg
        | Just mk <- mpat.fromNote
        , Just f <- Map.lookup n pENR_pENR ->
            fmap (mk . f) <$> eval_ppat (mkMondoPat getNote) arg
        | Just mk <- mpat.fromNote
        , Just f <- Map.lookup n pENum_pENum ->
            fmap (mk . f) <$> eval_ppat (mkMondoPat getNote) arg
    MList [Com n, MValue (Pos v)]
        | Just mk <- mpat.fromInt
        , Just f <- Map.lookup n int_pInt ->
            pure $ (1, mk (f $ round v))
    -- see Note [Chaining Functions Locally]
    MList xs | Just fromControl <- mpat.fromControl -> (1,) <$> fromControl <$> eval_list (env{currentParam = mpat.localExpr}) xs
    -- a def variable
    Com n | Just v <- lookup n env.defs -> eval_ppat mpat v
    -- this is a value, make it a pattern.
    _ | Just v <- mpat.exprToPat expr -> pure $ (1, withHighlight $ mpat.patToControl v)
    _ -> mkError ("unexpected pat: " <> show expr) (exprPos expr)
  where
    withHighlight
        | highlight = id
        | -- Always highlight key params
          Just (Com n) <- mpat.localExpr
        , n `elem` ["note", "n", "sound", "s"] =
            id
        | otherwise = T.withContext (\c -> c{T.contextPosition = []})
    eval_ppat :: (T.Parseable a, T.Enumerable a, Ord a) => MondoPat a b -> MondoExpr -> Either ParseError (Rational, T.Pattern b)
    eval_ppat = eval_pat highlight env
    eval_op getp appOp param val = do
        paramPat <- snd <$> eval_ppat (mkMondoPat getp) param
        (l, valPat) <- eval_ppat mpat val
        pure (l, appOp paramPat valPat)

isSound :: Maybe MondoExpr -> Bool
isSound me = case me of
    Just (Com "s") -> True
    Just (Com "sound") -> True
    _ -> False

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
