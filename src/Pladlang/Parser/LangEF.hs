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
--    ( parseExpr
--    )
where

import Pladlang.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void

type Parser = Parsec Void Text

-- | Parse Language EF syntax.
parseExpr :: Parser Expr
parseExpr = pExpr <* eof

pExpr :: Parser Expr
pExpr =
    pExprBinOp
    <|> pExprNoLookahead
    <|> EVar <$> lexeme pExprVar

pExprNoLookahead :: Parser Expr
pExprNoLookahead =
    ENum <$> lexeme L.decimal
    <|> EStr <$> lexeme (symbol "\"" *> pExprStrTextAnyPrintable)
    <|> ETrue <$ symbol "true"
    <|> EFalse <$ symbol "false"
    <|> ELen <$> lexeme (symbol "|" *> pExpr <* symbol "|")
    <|> lexeme pExprIf
    <|> lexeme pExprLet
    <|> lexeme pExprLam
    <|> EAp <$> lexeme (brackets pExpr) <*> lexeme (brackets pExpr)

pExprBinOp :: Parser Expr
pExprBinOp =
    try (lexeme (pExprBinOp' EPlus "plus" "+"))
    <|> try (lexeme (pExprBinOp' ETimes "times" "*"))
    <|> try (lexeme (pExprBinOp' ECat "cat" "++"))
    <|> try (lexeme (pExprBinOp' EEqual "equal" "="))

pExprBinOp' f name op = do
    e1 <- pExprNoLookahead
    symbol op
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
            return $ T.cons c str

-- holy shit LOL
pExprVar :: Parser Text
pExprVar = ((<>) . T.singleton) <$> letterChar <*> (T.pack <$> many alphaNumChar)

pExprIf :: Parser Expr
pExprIf = do
    symbol "if"
    e <- pExpr
    symbol "then"
    e1 <- pExpr
    symbol "else"
    e2 <- pExpr
    return $ EIf e e1 e2

pExprLet :: Parser Expr
pExprLet = do
    symbol "let"
    x <- pExprVar
    symbol "be"
    e1 <- pExpr
    symbol "in"
    e2 <- pExpr
    return $ ELet e1 x e2

pExprLam :: Parser Expr
pExprLam = do
    symbol "\\"
    symbol "("
    x <- pExprVar
    symbol ":"
    t <- pType
    symbol ")"
    symbol "."
    symbol "("
    e <- pExpr
    symbol ")"
    return $ ELam t x e

pType :: Parser Type
pType = try pArrowType <|> pPlainTypes

pPlainTypes :: Parser Type
pPlainTypes =
    TNum <$ symbol "num"
    <|> TStr <$ symbol "str"
    <|> TBool <$ symbol "bool"

pArrowType :: Parser Type
pArrowType = do
    t1 <- pPlainTypes
    symbol "->"
    t2 <- pType
    return $ TArrow t1 t2

------------------------------------------------------------
sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "--")
    empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

lStr = lexeme . string

betweenSymbols :: Text -> Text -> Parser a -> Parser a
betweenSymbols start end = between (symbol start) (symbol end)

quotes :: Parser a -> Parser a
quotes = betweenSymbols "\"" "\""

brackets :: Parser a -> Parser a
brackets = betweenSymbols "(" ")"

squareBrackets :: Parser a -> Parser a
squareBrackets = betweenSymbols "[" "]"

curlyBrackets :: Parser a -> Parser a
curlyBrackets = betweenSymbols "{" "}"

test = parseTest parseExpr
testT = parseTest (pType <* eof)
