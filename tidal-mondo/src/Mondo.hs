module Mondo (
    -- * Parser
    mondoToTidal,
    mondoToExpr,

    -- * Data
    MondoExpr (..),
    Positioned (..),

    -- * Debug
    showAst,
) where

import Sound.Tidal.Pattern (ControlPattern)
import Text.Parsec (ParseError)

import Mondo.Eval (eval)
import Mondo.Parser (MondoExpr (..), parse, showAst)
import Mondo.Token (Positioned (..), tokenize)

-- | Convert mondo notation to tidal pattern.
mondoToTidal :: String -> Either ParseError ControlPattern
mondoToTidal s = mondoToExpr s >>= eval

-- | Convert mondo notation to low-level 'MondoExpr'
mondoToExpr :: String -> Either ParseError MondoExpr
mondoToExpr s = tokenize s >>= parse
