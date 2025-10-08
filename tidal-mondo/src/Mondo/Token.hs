{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}

module Mondo.Token (MondoToken (..), Positioned (..), pattern Pos, tokenize) where

import Control.Applicative ((<|>))
import Data.Char (isSpace)
import Data.Functor.Identity (Identity)
import Text.Parsec qualified as P

data MondoToken
    = QuotesDouble String
    | OpenToken Char
    | CloseToken Char
    | Plain String
    | Number Float
    deriving (Eq, Show)

type TokenP a = P.ParsecT String () Identity a

pattern Pos :: a -> Positioned a
pattern Pos v <- Positioned v _ _

-- | A positioned value, for event contextLocation.
data Positioned a = Positioned
    { value :: a
    , col :: Int
    , row :: Int
    }
    deriving (Functor, Show, Eq)

quoteP :: Char -> TokenP String
quoteP c = P.between (P.char c) (P.char c) (P.many (P.satisfy (/= c)))

numberP :: TokenP Float
numberP = rd <$> float
  where
    rd = read :: String -> Float
    (<++>) a b = (++) <$> a <*> b
    (<:>) a b = (:) <$> a <*> b
    number = P.many1 P.digit
    dotNumber = P.try $ do
        v <- P.char '.' <:> number
        pure $ '0' : v
    plus = P.char '+' *> number
    minus = P.char '-' <:> number
    integer = plus <|> minus <|> dotNumber <|> number
    float = integer <++> decimal <++> expo
      where
        decimal = P.option "" $ P.try $ P.char '.' <:> number
        expo = P.option "" $ P.oneOf "eE" <:> integer

ops :: String
ops = "*/:!@%&?+-"

spacesP :: TokenP ()
spacesP = P.skipMany (spaces <|> oneLineComment)
  where
    spaces = P.skipMany1 (P.satisfy isSpace)
    oneLineComment = do
        _ <- P.try (P.string "//")
        P.skipMany (P.satisfy (/= '\n'))
        pure ()

tokenP :: TokenP MondoToken
tokenP =
    QuotesDouble <$> quoteP '"'
        <|> Plain <$> quoteP '\''
        <|> OpenToken <$> P.oneOf "(<[{"
        <|> CloseToken <$> P.oneOf ")>]}"
        <|> Number <$> numberP
        <|> Plain <$> P.many1 (P.letter <|> P.digit <|> P.oneOf "-~_^#") -- identifier
        <|> Plain . (: []) <$> P.oneOf ops
        <|> Plain <$> P.string ".." -- range
        <|> Plain . (: []) <$> P.oneOf ",$|#" -- stack, or, pipe

positionedTokenP :: TokenP (Positioned MondoToken)
positionedTokenP = do
    pos <- P.getPosition
    token <- tokenP
    spacesP
    pure $ Positioned token (P.sourceColumn pos) (P.sourceLine pos)

tokenize :: String -> Either P.ParseError [Positioned MondoToken]
tokenize = P.runParser (spacesP *> P.many positionedTokenP <* P.eof) () "input"
