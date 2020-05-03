-- | A parser for a very simple lambda calculus-like language named Language EF.
--
-- Written according to the provided concrete syntax rules.
--
-- I'm not good at writing parsers and I've never used megaparsec, so the
-- implementation is extremely messy.
--
-- The language reference didn't specify any rules for parsing, just syntax. No
-- operator precedences, associativity. And I got utterly lost trying to learn
-- about it, so... all operators are equal (cool, good), and all
-- right-associative (LOL). There are no brackets, either, so you can't change
-- that.
--
-- The language is adapted from: /Harper, Robert. Practical foundations for
-- programming languages. Cambridge University Press, 2016./

{-# LANGUAGE OverloadedStrings #-}

module Pladlang.Parser.LangEF
    ( pExpr
    , pType
    ) where

import Pladlang.AST
import Pladlang.Parser.Utils
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import qualified Data.Text as T

pExpr :: Parser Expr
pExpr =
    pExprBinOp
    <|> pExprNoLookahead
    <|> EVar <$> lexeme pExprVar

pExprNoLookahead :: Parser Expr
pExprNoLookahead =
    ENum <$> lexeme L.decimal
    <|> EStr <$> (charLexeme '\\' *> pExprStrTextAnyPrintable)
    <|> ETrue <$ strLexeme "true"
    <|> EFalse <$ strLexeme "false"
    -- <|> ELen <$> lexeme (charLexeme '|' *> pExpr <* charLexeme '|')
    <|> ELen <$> betweenCharLexemes '|' '|' pExpr
    <|> pExprIf
    <|> pExprLet
    <|> pExprLam
    <|> EAp <$> brackets pExpr <*> brackets pExpr

pExprBinOp :: Parser Expr
pExprBinOp =
    try (pExprBinOp' EPlus "plus" "+")
    <|> try (pExprBinOp' ETimes "times" "*")
    <|> try (pExprBinOp' ECat "cat" "++")
    <|> try (pExprBinOp' EEqual "equal" "=")

pExprBinOp' f name op = do
    e1 <- pExprNoLookahead
    strLexeme op
    e2 <- pExpr
    return $ f e1 e2

-- yeah lol
-- consumes up to a quote (eats the quote too but not returned)
pExprStrTextAnyPrintable :: Parser Text
pExprStrTextAnyPrintable = do
    c <- printChar
    case c of
        '\"' -> return ""
        _ -> do
            str <- pExprStrTextAnyPrintable
            lexeme $ return $ T.cons c str

-- holy shit LOL
pExprVar :: Parser Text
pExprVar = ((<>) . T.singleton) <$> letterChar <*> (T.pack <$> lexeme (many alphaNumChar))

pExprIf :: Parser Expr
pExprIf = do
    strLexeme "if"
    e <- pExpr
    strLexeme "then"
    e1 <- pExpr
    strLexeme "else"
    e2 <- pExpr
    return $ EIf e e1 e2

pExprLet :: Parser Expr
pExprLet = do
    strLexeme "let"
    x <- pExprVar
    strLexeme "be"
    e1 <- pExpr
    strLexeme "in"
    e2 <- pExpr
    return $ ELet e1 x e2

pExprLam :: Parser Expr
pExprLam = do
    charLexeme '\\'
    charLexeme '('
    x <- pExprVar
    charLexeme ':'
    t <- pType
    charLexeme ')'
    charLexeme '.'
    charLexeme '('
    e <- pExpr
    charLexeme ')'
    return $ ELam t x e

pType :: Parser Type
pType = try pArrowType <|> pPlainTypes

pPlainTypes :: Parser Type
pPlainTypes =
    TNum <$ strLexeme "num"
    <|> TStr <$ strLexeme "str"
    <|> TBool <$ strLexeme "bool"

pArrowType :: Parser Type
pArrowType = do
    t1 <- pPlainTypes
    strLexeme "->"
    t2 <- pType
    return $ TArrow t1 t2
