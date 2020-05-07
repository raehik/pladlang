{-# LANGUAGE OverloadedStrings #-}

module Pladlang.Parser.SyntaxSelect
    ( pSyntaxSelectBlocks
    ) where

import Pladlang.AST
import Pladlang.Parser.Utils
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Control.Applicative.Combinators.NonEmpty as CNE
import Data.Semigroup
import qualified Pladlang.Parser.LangEF as Parser.LangEF
import qualified Pladlang.Parser.LangEFAST as Parser.LangEFAST

--------------------------------------------------------------------------------
-- | Parse a non-empty list of synxtax-annotated blocks, and flatten.
pSyntaxSelectBlocks :: Parser (NonEmpty Expr)
pSyntaxSelectBlocks = sconcat <$> NE.some1 pSyntaxSelectBlock

-- | Parse a non-empty list of expressions for a single syntax-annotated block.
pSyntaxSelectBlock :: Parser (NonEmpty Expr)
pSyntaxSelectBlock = pKeyword "#syntax" *> pBlocks syntaxIdentifiers
  where
    pBlocks :: [(Text, Parser Expr)] -> Parser (NonEmpty Expr)
    pBlocks = choice . map (\(ident, p) -> pKeyword ident *> pExprList p)

-- | A mapping between valid syntax identifiers and their respective expression
--   parsers.
syntaxIdentifiers :: [(Text, Parser Expr)]
syntaxIdentifiers =
    [ ( "ef",     Parser.LangEF.pExpr )
    , ( "ef-ast", Parser.LangEFAST.pExpr )
    ]

-- | Parse a non-empty list of expressions, separated by semicolons.
--
-- Note that semicolons require a following expression. The final expression
-- must not have a trailing semicolon. For this reason, I suggest using a prefix
-- style like so:
--
-- >   expr 1
-- > ; expr 2
-- > ; expr 3
pExprList :: Parser Expr -> Parser (NonEmpty Expr)
pExprList p = CNE.sepBy1 p (charLexeme ';')
