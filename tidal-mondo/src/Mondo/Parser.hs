{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Mondo.Parser where

import Data.Functor.Identity (Identity)
import Data.List (intercalate)
import Mondo.Token
import Text.Parsec qualified as P

-- | The core mondo expr
data MondoExpr
    = -- | Function call, or pattern arg
      MList [MondoExpr]
    | -- | Special value injected through sugar, not to be confused with user defined values.
      MCommand String
    | -- | A plain value like "bd"
      MPlain (Positioned String)
    | -- | A plain number like ".42"
      MValue (Positioned Float)
    | -- | A double quoted string, to be parsed as tidal mini-notation
      MString (Positioned String)
    | -- | A lambda
      MLam [String] MondoExpr
    deriving (Show, Eq)

type Parser a = P.ParsecT [Positioned MondoToken] () Identity a

showAst :: MondoExpr -> String
showAst e = case e of
    MList xs -> "(" <> intercalate " " (map showAst xs) <> ")"
    MPlain p -> p.value
    MString s -> s.value
    MCommand c -> c
    MValue p -> case properFraction p.value of
        (n, 0) -> show (n :: Int)
        (0, _) -> drop 1 $ show p.value
        _ -> show p.value
    MLam as l -> "fn (" <> unwords as <> ") " <> showAst l

parse :: [Positioned MondoToken] -> Either P.ParseError MondoExpr
parse xs = ensureList . desugar <$> P.runParser (MList <$> P.many mondoP) () "input" xs

ensureList :: MondoExpr -> MondoExpr
ensureList (MList x) = MList x
ensureList x = MList [x]

desugar :: MondoExpr -> MondoExpr
desugar (MList (MPlain (Pos "$") : rest)) = desugar $ MList rest
desugar (MList [MPlain (Pos "#")]) = MLam ["_"] (MCommand "_")
desugar (MList [x]) = desugar x
desugar (MList [MCommand "square", x]) = desugar x
desugar (MList [MCommand "angle", x]) = desugar x
desugar (MList xs) = case desugar_nested $ map desugar $ desugar_list xs of
    [MList ds] -> MList ds
    ds -> MList ds
desugar x = x

desugar_nested :: [MondoExpr] -> [MondoExpr]
desugar_nested [MCommand "stack", x, MList (MCommand "stack" : rest)] = MCommand "stack" : x : rest
desugar_nested x = x

desugar_list :: [MondoExpr] -> [MondoExpr]
desugar_list = desugar_pipes [] . desugar_ands . desugar_ops . desugar_or . desugar_stack

desugar_ands :: [MondoExpr] -> [MondoExpr]
desugar_ands [] = []
desugar_ands (l : MPlain (Pos "&") : r : rest) = desugar_ands $ (MList [MCommand "&", l, r] : rest)
desugar_ands (x : xs) = x : desugar_ands xs

desugar_ops :: [MondoExpr] -> [MondoExpr]
desugar_ops [] = []
desugar_ops (l : MPlain (Pos v) : r : rest) | v `elem` ["*", "/", ":", "@", "!", ".."] = desugar_ops $ (MList [MCommand v, r, l] : rest)
desugar_ops (x : xs) = x : desugar_ops xs

desugar_stack :: [MondoExpr] -> [MondoExpr]
desugar_stack xs =
    let (left, rest) = span (isSplit [",", "$"]) xs
        addLeftAp
            | v : _ <- left, MCommand n <- v, n `elem` ["square", "angle"] = (v :)
            | otherwise = id
     in case rest of
            [] -> left
            (_ : right) -> [MCommand "stack", MList left, MList (desugar_stack $ addLeftAp right)]

desugar_or :: [MondoExpr] -> [MondoExpr]
desugar_or xs =
    let (left, rest) = span (isSplit ["|"]) xs
     in case rest of
            [] -> left
            (_ : right) -> [MCommand "or", MList left, MList (desugar_or right)]

desugar_pipes :: [MondoExpr] -> [MondoExpr] -> [MondoExpr]
desugar_pipes acc xs =
    let (left, rest) = span (isSplit ["#"]) xs
        leftAcc = case acc of
            [] -> left
            _ -> left <> [MList acc]
     in case rest of
            [] -> leftAcc
            _ : right -> case left of
                [] -> [MLam ["_"] $ MList $ desugar_pipes leftAcc (add_arg right)]
                _ -> desugar_pipes leftAcc right
  where
    add_arg rest =
        let (left, right) = span (isSplit ["#"]) rest
         in left <> [MCommand "_"] <> right

isSplit :: [String] -> MondoExpr -> Bool
isSplit s e = case e of
    MPlain v -> v.value `notElem` s
    _ -> True

mondoP :: Parser MondoExpr
mondoP = do
    next <- P.anyToken
    case next.value of
        Plain s -> pure $ MPlain (const s <$> next)
        OpenToken '(' -> MList <$> P.manyTill mondoP (closingP ')')
        OpenToken '[' -> mkList "square" <$> P.manyTill mondoP (closingP ']')
        OpenToken '<' -> mkList "angle" <$> P.manyTill mondoP (closingP '>')
        OpenToken _ -> error "The impossible has happened"
        Number n -> pure $ MValue (const n <$> next)
        QuotesDouble s -> pure $ MString (const s <$> next)
        CloseToken c -> fail $ "Unexpected closing token: " <> [c]

mkList :: String -> [MondoExpr] -> MondoExpr
mkList name = MList . (MCommand name :)

closingP :: Char -> Parser ()
closingP c = P.tokenPrim show (\p _ _ -> p) match
  where
    match next = case next.value of
        CloseToken c' | c == c' -> pure ()
        _ -> fail $ "Expecting: " <> show c
