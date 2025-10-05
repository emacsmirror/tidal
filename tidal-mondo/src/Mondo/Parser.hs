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
    | -- | Special function like "square" or "angle"
      MCommand String
    | -- | A plain value like "bd"
      MPlain (Positioned String)
    | -- | A plain number like ".42"
      MValue (Positioned Float)
    | -- | A double quoted string, to be parsed as tidal mini-notation
      MString (Positioned String)
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

parse :: [Positioned MondoToken] -> Either P.ParseError MondoExpr
parse xs = ensureList . desugar <$> P.runParser (MList <$> P.many mondoP) () "input" xs

ensureList :: MondoExpr -> MondoExpr
ensureList (MList x) = MList x
ensureList x = MList [x]

desugar :: MondoExpr -> MondoExpr
desugar (MList (MPlain (Pos "$") : rest)) = desugar $ MList rest
desugar (MList [x]) = desugar x
desugar (MList [MCommand "square", x]) = desugar x
desugar (MList [MCommand "angle", x]) = desugar x
desugar (MList xs) = case desugar_nested $ map desugar $ desugar_list xs of
    [MList ds] -> MList ds
    ds -> MList ds
desugar x = x

desugar_nested :: [MondoExpr] -> [MondoExpr]
desugar_nested [MCommand p, x, MList (MCommand q : rest)] | p == q = MCommand p : x : rest
desugar_nested x = x

desugar_list :: [MondoExpr] -> [MondoExpr]
desugar_list = desugar_pipes . desugar_ops . desugar_or . desugar_stack

desugar_ops :: [MondoExpr] -> [MondoExpr]
desugar_ops [] = []
desugar_ops (l : MPlain (Pos [v]) : r : rest) | v `elem` ['*', '/', ':'] = desugar_ops $ (MList [MCommand [v], r, l] : rest)
desugar_ops (x : xs) = x : desugar_ops xs

desugar_stack :: [MondoExpr] -> [MondoExpr]
desugar_stack xs =
    let (left, rest) = span (isSplit [",", "$"]) xs
        addLeftAp
            | v : _ <- left, MCommand n <- v, n `elem` ["square", "angle"] = (v :)
            | otherwise = id
     in case rest of
            [] -> left
            (_ : right) -> [MCommand "stack", MList left, MList (addLeftAp $ desugar_stack right)]

desugar_or :: [MondoExpr] -> [MondoExpr]
desugar_or xs =
    let (left, rest) = span (isSplit ["|"]) xs
     in case rest of
            [] -> left
            (_ : right) -> [MCommand "or", MList left, MList (desugar_or right)]

desugar_pipes :: [MondoExpr] -> [MondoExpr]
desugar_pipes xs =
    let (right, rest) = span (isSplit ["#"]) (reverse xs)
     in case rest of
            [] -> reverse right
            (_ : left) -> reverse right <> [MList $ desugar_pipes $ reverse left]

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
