-- | A parser for a very simple lambda calculus-like language named Language EF.
--
-- Written according to the concrete syntax rules provided to us with the
-- language definition, along with a few assumptions.
--
-- The language is adapted from: /Harper, Robert. Practical foundations for
-- programming languages. Cambridge University Press, 2016./

{-# LANGUAGE OverloadedStrings #-}

module Pladlang.Parser.LangEF
    ( parse
    )
where

import Pladlang.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

-- | Parse Language EF syntax.
--
-- The only
parse :: Parser Expr
parse = pExpr <* eof

pExpr :: Parser Expr
pExpr = pExprFunction <|> pExprPlainVal

pExprFunction :: Parser Expr
pExprFunction =
    pExprBinaryFunc EPlus "plus"
    <|> pExprBinaryFunc ETimes "times"
    <|> pExprBinaryFunc ECat "cat"
    <|> pExprUnaryFunc ELen "len"
    <|> pExprBinaryFunc EEqual "equal"
    <|> pExprIf
    <|> pExprLet
    <|> pExprLam
    <|> pExprBinaryFunc EAp "ap"

pExprPlainVal :: Parser Expr
pExprPlainVal =
    ENum <$> (string "num" >> squareBrackets L.decimal)
    <|> EStr . T.pack <$> (string "str" >> squareBrackets (some alphaNumChar))
    <|> ETrue <$ string "true"
    <|> EFalse <$ string "false"
    <|> EVar <$> pExprVar

pExprVar :: Parser Text
pExprVar = do
    start <- letterChar
    rest <- optional (some alphaNumChar)
    return $ T.singleton start <> maybe mempty T.pack rest

pExprIf :: Parser Expr
pExprIf = do
    _ <- string "if"
    _ <- char '('
    e <- pExpr
    _ <- char ';'
    e1 <- pExpr
    _ <- char ';'
    e2 <- pExpr
    _ <- char ')'
    return $ EIf e e1 e2

pExprLet :: Parser Expr
pExprLet = do
    _ <- string "let"
    _ <- char '('
    e1 <- pExpr
    _ <- char ';'
    x <- pExprVar
    _ <- char '.'
    e2 <- pExpr
    _ <- char ')'
    return $ ELet e1 x e2

pExprLam :: Parser Expr
pExprLam = do
    _ <- string "lam"
    t <- curlyBrackets pType
    _ <- char '('
    x <- pExprVar
    _ <- char '.'
    e <- pExpr
    _ <- char ')'
    return $ ELam t x e

pType :: Parser Type
pType =
    TNum <$ string "num"
    <|> TStr <$ string "str"
    <|> TBool <$ string "bool"
    <|> TArrow <$> pType <*> pType

------------------------------------------------------------
parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

squareBrackets :: Parser a -> Parser a
squareBrackets = between (char '[') (char ']')

curlyBrackets :: Parser a -> Parser a
curlyBrackets = between (char '{') (char '}')

pExprBinaryFunc :: (Expr -> Expr -> Expr) -> Text -> Parser Expr
pExprBinaryFunc f name = do
    _ <- string name
    _ <- char '('
    e1 <- pExpr
    _ <- char ';'
    e2 <- pExpr
    _ <- char ')'
    return $ f e1 e2

pExprUnaryFunc :: (Expr -> Expr) -> Text -> Parser Expr
pExprUnaryFunc f name = f <$> (string name >> parens pExpr)
