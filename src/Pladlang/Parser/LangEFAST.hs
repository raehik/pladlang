-- | A parser for a very simple lambda calculus-like language named Language EF.
--
-- AST-like syntax (already a tree).
--
-- Written according to the provided abstract syntax rules.
--
-- The language is adapted from: /Harper, Robert. Practical foundations for
-- programming languages. Cambridge University Press, 2016./

{-# LANGUAGE OverloadedStrings #-}

module Pladlang.Parser.LangEFAST
--    ( pType
--    ) where
                                 where

import           Pladlang.AST
import           Pladlang.Parser.Utils
import qualified Text.Megaparsec.Char.Lexer    as L
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Control.Applicative     hiding ( many )

pExpr :: Parser Expr
pExpr =
    ENum
        <$> strBeforeSquareBrackets "num" L.decimal
        <|> pExprStr
        <|> ETrue
        <$  pKeyword "true"
        <|> EFalse
        <$  pKeyword "false"
        <|> pFunction2 "plus"  EPlus  pExpr
        <|> pFunction2 "times" ETimes pExpr
        <|> pFunction2 "cat"   ECat   pExpr
        <|> pFunction1 "len" ELen pExpr
        <|> pFunction2 "equal" EEqual pExpr
        <|> pFunction3 "if" EIf pExpr
        <|> pFunction2 "ap" EAp pExpr
        <|> pExprLet'
        <|> pExprLam
        <|> EVar
        <$> pVar

pExprStr :: Parser Expr
pExprStr = do
    pKeyword "str"
    _   <- char '['
    str <- someTill printChar (charLexeme ']')
    return $ EStr . T.pack $ str

pExprLet :: Parser Expr
pExprLet = do
    pKeyword "let"
    charLexeme '('
    e1 <- pExpr
    charLexeme ';'
    x <- pVar
    charLexeme '.'
    e2 <- pExpr
    charLexeme ')'
    return $ ELet e1 x e2

pKw = pKeyword
pExprLet' :: Parser Expr
pExprLet' =
    --(\(e1, (x, e2)) -> ELet e1 x e2) <$> (pKw "let" *> brackets ((,) <$> pExpr `pSemicolonSep` pBindExpr))
            (\(e1, (x, e2)) -> ELet e1 x e2) <$> bracketedLet
  where
    bracketedLet :: Parser (Expr, (Text, Expr))
    bracketedLet = pKw "let" *> brackets (pExpr `pSemicolonSep` pBindExpr)

pSemicolonSep :: Parser a -> Parser b -> Parser (a, b)
pSemicolonSep pL pR = liftA2 (,) pL (charLexeme ';' *> pR)
pBindExpr :: Parser (Text, Expr)
pBindExpr = liftA2 (,) pVar (charLexeme '.' *> pExpr)

pVar :: Parser Text
pVar =
    ((<>) . T.singleton)
        <$> letterChar
        <*> (T.pack <$> lexeme (many alphaNumChar))

pExprLam :: Parser Expr
pExprLam = do
    pKeyword "lam"
    charLexeme '{'
    t <- pType
    charLexeme '}'
    charLexeme '('
    x <- pVar
    charLexeme '.'
    e <- pExpr
    charLexeme ')'
    return $ ELam t x e

strBeforeSquareBrackets :: Text -> Parser a -> Parser a
strBeforeSquareBrackets str parser = pKeyword str *> squareBrackets parser

pType :: Parser Type
pType =
    TNum
        <$  pKeyword "num"
        <|> TStr
        <$  pKeyword "str"
        <|> TBool
        <$  pKeyword "bool"
        <|> pFunction2 "arr" TArrow pType

pFunction1 :: Text -> (a -> a) -> Parser a -> Parser a
pFunction1 name f parser = pKeyword name *> (f <$> brackets parser)

pFunction2 :: Text -> (a -> a -> a) -> Parser a -> Parser a
pFunction2 name f parser = do
    pKeyword name
    charLexeme '('
    p1 <- parser
    charLexeme ';'
    p2 <- parser
    charLexeme ')'
    return $ f p1 p2

pFunction3 :: Text -> (a -> a -> a -> a) -> Parser a -> Parser a
pFunction3 name f parser = do
    pKeyword name
    charLexeme '('
    p1 <- parser
    charLexeme ';'
    p2 <- parser
    charLexeme ';'
    p3 <- parser
    charLexeme ')'
    return $ f p1 p2 p3
