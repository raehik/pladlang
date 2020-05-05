{-# LANGUAGE OverloadedStrings #-}

module Pladlang.Parser.SyntaxSelect
    ( pSyntaxSelectBlocks
    ) where

import Pladlang.AST
import Pladlang.Parser.Utils
import Data.Text (Text)
import Data.List.NonEmpty ( NonEmpty( (:|) ), (<|) )
import qualified Data.List.NonEmpty as NE
import Data.Semigroup
import qualified Pladlang.Parser.LangEF as Parser.LangEF
import qualified Pladlang.Parser.LangEFAST as Parser.LangEFAST

------------------------------------------------------------
-- | Parse a non-empty list of synxtax-annotated blocks, and flatten.
pSyntaxSelectBlocks :: Parser (NonEmpty Expr)
pSyntaxSelectBlocks = sconcat <$> NE.some1 pSyntaxSelectBlock

-- | Parse a non-empty list of expressions for a single syntax block.
pSyntaxSelectBlock :: Parser (NonEmpty Expr)
pSyntaxSelectBlock = strLexeme "#syntax" *>
    (makeSyntaxParser Parser.LangEFAST.pExpr "ef-ast"
    <|> makeSyntaxParser Parser.LangEF.pExpr "ef")

-- | Helper function for making syntax parsers.
makeSyntaxParser :: Parser Expr -> Text -> Parser (NonEmpty Expr)
makeSyntaxParser parser id = strLexeme id *> NE.some1 parser
