{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module Mondo.Eval (eval) where

import Control.Monad (replicateM)
import Data.Map.Strict qualified as Map
import GHC.Float (float2Double)
import Sound.Tidal.Core ((#))
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
eval es = case eval_maths es of
    MList (MCommand "stack" : rest) ->
        let (defs, pats) = go [] [] rest
         in eval_top (Env Nothing Nothing defs) $ MList $ MCommand "stack" : pats
    pats -> eval_top newEnv pats
  where
    -- remove muted pattern and extract def expression.
    go defs acc [] = (defs, reverse acc)
    go defs acc (x : xs) = case x of
        MList [Com "def", Com k, v] -> go ((k, v) : defs) acc xs
        MList rest | isMuted rest -> go defs acc xs
        _ -> go defs (x : acc) xs

    -- top level patter
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

-- perform static math operations
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
    -- custom mondo functions to control how the pattern are applied to the chain
    Com n : MList param : MList rest : []
        | Just f <- Map.lookup n pC_pC_pC -> do
            xPat <- eval_list env param
            yPat <- eval_list env rest
            pure $ f yPat xPat
    -- scale is custom in mondo so that it can be used after the notes like 'note 0 # scale minor'
    Com "scale" : param : MList rest : [] -> eval_scale param rest
    Com "note" : param : MList rest : [] -> eval_notes param rest
    Com "note" : param : [] -> eval_notes param []
    -- ControlPatterns like 'sound'
    Com n : param : rest
        | Just f <- Map.lookup n pStr_pC -> eval_control (mkMondoParam n PStr f) param rest
        | Just f <- Map.lookup n pDouble_pC -> eval_control (mkMondoParam n PDouble f) param rest
        | Just f <- Map.lookup n pInt_pC -> eval_control (mkMondoParam n PInt f) param rest
        | Just f <- Map.lookup n pNote_pC -> eval_control (mkMondoParam n PNote f) param rest
    -- Generic p* control patterns
    Com p : Com name : param : rest
        | p == "pF" -> eval_control (mkMondoParam name PDouble (T.pF name)) param rest
        | p == "pI" -> eval_control (mkMondoParam name PInt (T.pI name)) param rest
        | p == "pN" -> eval_control (mkMondoParam name PNote (T.pN name)) param rest
        | p == "pS" -> eval_control (mkMondoParam name PStr (T.pS name)) param rest
        | p == "pR" -> eval_control (mkMondoParam name PTime (T.pR name)) param rest
        | p == "pB" -> eval_control (mkMondoParam name PBool (T.pB name)) param rest
    -- ControlPatterns with multiple params
    Com n : MValue x : MList y : MList (Com "list" : z) : []
        | Just f <- Map.lookup n time_pC_ppC_pC -> do
            ypat <- eval_list env y
            zpat <- traverse (eval_top env) z
            pure $ f (toTime x.value) ypat zpat
    -- n-colon-pat is injected by the eval_pat, when evaluating expression like 'bd:<(1 # lpf 42)>'
    MCommand "n-colon-pat" : param : rest -> eval_control (mkMondoParam "" PDouble (T.pF "n")) param rest
    -- stack separate mondo pattern, it is injected with '$'
    MCommand "stack" : rest -> T.stack <$> traverse (eval_top env) rest
    -- modifiers
    _ | Just (xs, MList rest) <- unsnoc es -> do
        f <- eval_fun env $ MList xs
        f <$> eval_list env rest
    x : _ -> mkError ("unexpected command: " <> show es) (exprPos x)
    [] -> mkError "expected command!" (P.newPos "input" 1 1)
  where
    eval_ppat :: (T.Parseable a, T.Enumerable a, Ord a) => MondoPat a b -> MondoExpr -> Either ParseError (T.Pattern b)
    eval_ppat mpat expr = snd <$> eval_pat False env mpat expr

    eval_notes :: MondoExpr -> [MondoExpr] -> Either ParseError T.ControlPattern
    eval_notes param rest = do
        notePat <- case env.envScale of
            -- No scale defined, eval notes from pattern
            Nothing -> eval_ppat (mkMondoParam "note" PNote T.note) param
            -- Scale was piped, eval int from pattern
            Just scale -> eval_ppat (mkMondoParam "scale" PNote scale) param
        case rest of
            [] -> pure notePat
            xs -> do
                restPat <- eval_list env xs
                pure $ restPat # notePat

    eval_scale :: MondoExpr -> [MondoExpr] -> Either ParseError T.ControlPattern
    eval_scale param rest = do
        scalePat <- eval_ppat (mkMondoPat PStr) param
        case eval_pat False env (mkMondoPat PInt) (MList rest) of
            -- rest are notes, apply the scale and make a control param
            Right (_, notePat) -> pure $ T.note $ T.scale scalePat notePat
            -- rest is probably a pipe, add the scale to the env, and apply it later when encountering notes.
            Left _ -> eval_list (env{envScale = Just (T.note . T.scale scalePat . fmap noteToInt)}) rest

    -- Evaluate a control pattern like 's bd'
    eval_control :: (T.Parseable a, T.Enumerable a, Ord a) => MondoPat a T.ValueMap -> MondoExpr -> [MondoExpr] -> Either ParseError (T.ControlPattern)
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

eval_fun :: Env -> MondoExpr -> Either ParseError (T.ControlPattern -> T.ControlPattern)
eval_fun env expr = case expr of
    MList [MLam _ body] -> eval_fun env body
    -- Direct modifiers like 'rev'
    MList (Com n : rest)
        | Just f <- Map.lookup n pA_pA -> eval_compo f rest
        | Just f <- Map.lookup n pC_pC -> eval_compo f rest
    -- Modifiers with literal param
    MList (Com n : MValue v : rest)
        | Just f <- Map.lookup n time_pC_pC -> eval_compo (f $ toTime v.value) rest
        | Just f <- Map.lookup n time_pA_pA -> eval_compo (f $ toTime v.value) rest
    MList (Com n : MValue x : MValue y : rest)
        | Just f <- Map.lookup n int_time_pA_pA -> eval_compo (f (round x.value) (toRational y.value)) rest
    -- Modifiers with one param
    MList (Com n : x : rest)
        | Just f <- Map.lookup n pTime_pA_pA -> eval_mod PTime f x rest
        | Just f <- Map.lookup n pTime_pC_pC -> eval_mod PTime f x rest
        | Just f <- Map.lookup n pBool_pA_pA -> eval_mod PBool f x rest
        | Just f <- Map.lookup n pS_pA_pA -> eval_mod PStr f x rest
        | Just f <- Map.lookup n pInt_pOrd_pOrd -> eval_mod PInt f x rest
        | Just f <- Map.lookup n pInt_pA_pA -> eval_mod PInt f x rest
        | Just f <- Map.lookup n pInt_pC_pC -> eval_mod PInt f x rest
        | Just f <- Map.lookup n pCpC_pC_pC -> eval_fmod f x rest
        | Just f <- Map.lookup n pApA_pA_pA -> eval_fmod f x rest
        | Just f <- Map.lookup n pC_pC_pC
        , MList c1 <- x -> do
            pc1 <- eval_list env c1
            eval_compo (flip f pc1) rest
    -- Modifiers with two params
    MList (Com n : param1 : param2 : rest)
        | Just f <- Map.lookup n pTime_pTime_pA_pA -> do
            pat1 <- eval_ppat (mkMondoPat PTime) param1
            eval_mod PTime (f pat1) param2 rest
        | Just f <- Map.lookup n pInt_pInt_pC_pC -> do
            pat1 <- eval_ppat (mkMondoPat PInt) param1
            eval_mod PInt (f pat1) param2 rest
        | Just f <- Map.lookup n pInt_pDouble_pC_pC -> do
            pat1 <- eval_ppat (mkMondoPat PInt) param1
            eval_mod PDouble (f pat1) param2 rest
        | Just f <- Map.lookup n pDouble_pCpC_pC_pC -> do
            pat1 <- eval_ppat (mkMondoPat PDouble) param1
            g <- eval_fun env param2
            eval_compo (f pat1 g) rest
        | Just f <- Map.lookup n pDouble_pApA_pA_pA -> do
            pat1 <- eval_ppat (mkMondoPat PDouble) param1
            g <- eval_fun env param2
            eval_compo (f pat1 g) rest
        | Just f <- Map.lookup n pInt_ppTime_pC_pC
        , MList (Com "list" : xs) <- param2 -> do
            nPat <- eval_ppat (mkMondoPat PInt) param1
            tpats <- traverse (eval_ppat (mkMondoPat PTime)) xs
            eval_compo (f nPat tpats) rest
        | Just f <- Map.lookup n pInt_pApA_pA_pA -> do
            npat <- eval_ppat (mkMondoPat PInt) param1
            fa <- eval_fun env param2
            eval_compo (f npat fa) rest
        | Just f <- Map.lookup n pTime_pApA_pA_pA -> do
            npat <- eval_ppat (mkMondoPat PTime) param1
            fa <- eval_fun env param2
            eval_compo (f npat fa) rest
    -- Modifiers with three params
    MList (Com n : param1 : param2 : param3 : rest)
        | Just f <- Map.lookup n pInt_pTime_pDouble_pC_pC -> do
            pat1 <- eval_ppat (mkMondoPat PInt) param1
            pat2 <- eval_ppat (mkMondoPat PTime) param2
            eval_mod PDouble (f pat1 pat2) param3 rest
    MList (Com n : p1 : MValue p2 : p3 : p4 : rest)
        | Just f <- Map.lookup n pTime_int_pInt_pInt_pA_pA -> do
            pat1 <- eval_ppat (mkMondoPat PTime) p1
            pat3 <- eval_ppat (mkMondoPat PInt) p3
            pat4 <- eval_ppat (mkMondoPat PInt) p4
            eval_compo (f pat1 (round p2.value) pat3 pat4) rest
    MCommand "_" -> pure id
    Com _ -> eval_fun env (MList [expr])
    MList xs -> case eval_list env xs of
        Left _ -> mkError ("arg is not fun: " <> show expr) (exprPos expr)
        Right p -> pure (# p)
    _ -> mkError ("expected fun, got: " <> show expr) (exprPos expr)
  where
    eval_ppat :: (T.Parseable a, T.Enumerable a, Ord a) => MondoPat a b -> MondoExpr -> Either ParseError (T.Pattern b)
    eval_ppat mpat e = snd <$> eval_pat False env mpat e
    eval_mod :: (T.Parseable a, T.Enumerable a, Ord a) => Pat a -> (T.Pattern a -> T.ControlPattern -> T.ControlPattern) -> MondoExpr -> [MondoExpr] -> Either ParseError (T.ControlPattern -> T.ControlPattern)
    eval_mod get app param rest = do
        a <- eval_ppat (mkMondoPat get) param
        eval_compo (app a) rest
    eval_fmod :: ((T.ControlPattern -> T.ControlPattern) -> T.ControlPattern -> T.ControlPattern) -> MondoExpr -> [MondoExpr] -> Either ParseError (T.ControlPattern -> T.ControlPattern)
    eval_fmod app param rest = do
        f <- eval_fun env param
        eval_compo (app f) rest
    -- eval_compo :: (T.ControlPattern -> T.ControlPattern) -> [MondoExpr] -> Either ParseError (T.ControlPattern -> T.ControlPattern)
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
        yPat <- snd <$> eval_ppat (mkMondoPat PDouble) y
        fmap (T.degradeBy yPat) <$> eval_ppat mpat x
    -- x*y
    MList [MCommand "*", param, val] -> eval_op PTime T.fast param val
    -- x/y
    MList [MCommand "/", param, val] -> eval_op PTime T.slow param val
    -- range x y p
    MList [Com "range", x, y, p]
        | -- double range
          PDouble <- mpat.pat -> do
            let rpat = mkMondoPat PDouble
            xPat <- snd <$> eval_ppat rpat x
            yPat <- snd <$> eval_ppat rpat y
            (l, pPat) <- eval_ppat rpat p
            pure (l, mpat.patToControl $ T.range xPat yPat pPat)
        | -- note range
          PNote <- mpat.pat -> do
            let rpat = mkMondoPat mpat.pat
            xPat <- snd <$> eval_ppat rpat x
            yPat <- snd <$> eval_ppat rpat y
            (l, pPat) <- eval_ppat rpat p
            pure (l, mpat.patToControl $ T.range xPat yPat pPat)
    -- x..y
    MList [MCommand "..", y, x] -> do
        let rpat = mkMondoPat mpat.pat
        xPat <- snd <$> eval_ppat rpat x
        yPat <- snd <$> eval_ppat rpat y
        pure (1, mpat.patToControl $ T.unwrap $ T.fromTo <$> xPat <*> yPat)
    -- x:y
    MList [MCommand ":", x, y]
        | isSound mpat.localExpr
        , Just colonOp <- mpat.colonOp -> do
            let colonSoundPat = (mkMondoParam "" PDouble (T.pF "n")){localExpr = Just $ MCommand "n-colon-pat"}
            (l, soundPat) <- eval_ppat mpat y
            npat <- snd <$> eval_ppat colonSoundPat x
            pure (l, colonOp soundPat npat)
        | Just (Com "vib") <- mpat.localExpr
        , Just colonOp <- mpat.colonOp -> do
            (l, vibPat) <- eval_ppat mpat y
            vibmodPat <- snd <$> eval_ppat (mkMondoParam "" PDouble (T.pF "vibmod")) x
            pure (l, colonOp vibPat vibmodPat)
        | Just (Com "distort") <- mpat.localExpr
        , Just colonOp <- mpat.colonOp -> do
            (l, yPat) <- eval_ppat mpat y
            xPat <- snd <$> eval_ppat (mkMondoParam "" PDouble (T.pF "distortvol")) x
            pure (l, colonOp yPat xPat)
        | Just (MCommand "&") <- mpat.localExpr
        , Just andOp <- mpat.andOp -> do
            yPat <- snd <$> eval_ppat (mkMondoPat PInt) y
            zPat <- snd <$> eval_ppat (mkMondoPat PInt) x
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
        (l,) . snd <$> eval_ppat epat xs
    -- ~
    Com "~" -> pure (1, T.silence)
    -- Modifier with 1 pattern param, like fast or segment
    MList [Com n, arg, rest]
        | Just tmod <- Map.lookup n pTime_pA_pA -> eval_op PTime tmod arg rest
    MList [Com n, arg]
        | Just mk <- fromNote mpat
        , Just f <- Map.lookup n pInt_pNum ->
            fmap (mk . f) <$> eval_ppat (mkMondoPat PInt) arg
        | Just mk <- fromNote mpat
        , Just f <- Map.lookup n pENR_pENR ->
            fmap (mk . f) <$> eval_ppat (mkMondoPat PNote) arg
        | Just mk <- fromNote mpat
        , Just f <- Map.lookup n pENum_pENum ->
            fmap (mk . f) <$> eval_ppat (mkMondoPat PNote) arg
        | PDouble <- mpat.pat
        , Just f <- Map.lookup n pFrac_pFrac ->
            fmap (mpat.patToControl . f) <$> eval_ppat (mkMondoPat PDouble) arg
    MList [Com n, MValue (Pos v)]
        | Just mk <- fromInt mpat
        , Just f <- Map.lookup n int_pInt ->
            pure $ (1, mk (f $ round v))
    -- note mods
    MList [Com n, MValue x, rest]
        | PNote <- mpat.pat
        , Just f <- Map.lookup n realFrac_pA_pA -> do
            fmap (mpat.patToControl . f (T.Note $ float2Double x.value)) <$> eval_ppat (mkMondoPat PNote) rest
    -- see Note [Chaining Functions Locally]
    MList xs | Just fromControl <- mpat.fromControl -> (1,) <$> fromControl <$> eval_list (env{currentParam = mpat.localExpr}) xs
    -- a def variable
    Com n | Just v <- lookup n env.defs -> eval_ppat mpat v
    -- this is a value, make it a pattern.
    _ | Just v <- exprToPat mpat.pat expr -> pure $ (1, withHighlight $ mpat.patToControl v)
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

-- 'unsnoc' comes from base-4.19. TODO: remove when this become the minimum supported by tidal
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
