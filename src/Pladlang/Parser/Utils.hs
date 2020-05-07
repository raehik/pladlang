{-# LANGUAGE OverloadedStrings #-}

module Pladlang.Parser.Utils
    ( module Pladlang.Parser.Utils
    , module Text.Megaparsec
    , module Text.Megaparsec.Char
    ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import Data.Void

type Parser = Parsec Void Text

------------------------------------------------------------
-- | Consume spaces before a parser, and consume EOF after it.
topParse :: Parser a -> Parser a
topParse = between sc eof

pKeyword :: Text -> Parser ()
pKeyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar) *> pure ()

------------------------------------------------------------
-- | Space consumer.
sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "--")
    empty

-- | Generic wrapper for all lexemes (tokens) in the language.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Wrapper for string lexemes.
--strLexeme :: Text -> Parser Text
--strLexeme = lexeme . string
strLexeme :: Text -> Parser ()
strLexeme str = (lexeme . string) str *> pure ()

-- | Wrapper for char lexemes.
--charLexeme :: Char -> Parser Char
--charLexeme = lexeme . char
charLexeme :: Char -> Parser ()
charLexeme ch = (lexeme . char) ch *> pure ()

-- | Wrapper for parsers between string lexemes.
betweenKeywords :: Text -> Text -> Parser a -> Parser a
betweenKeywords start end = between (pKeyword start) (pKeyword end)

-- | Wrapper for parsers between char lexemes.
betweenCharLexemes :: Char -> Char -> Parser a -> Parser a
betweenCharLexemes start end = between (charLexeme start) (charLexeme end)

--------------------------------------------------------------------------------
quotes :: Parser a -> Parser a
quotes = betweenCharLexemes '"' '"'

brackets :: Parser a -> Parser a
brackets = betweenCharLexemes '(' ')'

squareBrackets :: Parser a -> Parser a
squareBrackets = betweenCharLexemes '[' ']'

curlyBrackets :: Parser a -> Parser a
curlyBrackets = betweenCharLexemes '{' '}'
