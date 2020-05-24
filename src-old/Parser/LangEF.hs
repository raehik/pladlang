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
    )
where

import           Pladlang.AST
import           Pladlang.Parser.Utils
import qualified Text.Megaparsec.Char.Lexer    as L
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Control.Monad.Combinators.Expr
import           Control.Applicative     hiding ( many )

pExpr :: Parser Expr
pExpr = makeExprParser (try term) table <?> "expression"

keywords = ["if", "then", "else", "let", "be", "in", "true", "false"]


term :: Parser Expr
term =
    choice
            [ brackets pExpr
            , EStr <$> pStringLiteral
            , ENum <$> lexeme L.decimal
            , ETrue <$ pKeyword "true"
            , EFalse <$ pKeyword "false"
            , ELen <$> betweenCharLexemes '|' '|' pExpr
            , pExprIf
            , pExprLet
            , pExprLam
            , pExprVar
            , pExprMetaVar
            ]
        <?> "term"

pStringLiteral :: Parser Text
pStringLiteral =
    T.pack <$> (char '"' *> manyTill L.charLiteral (charLexeme '"'))

table :: [[Operator Parser Expr]]
table =
    [ [binaryEmpty EAp]
    , [binaryCh '*' ETimes]
    , [binaryCh '+' EPlus]
    , [binaryStr "++" ECat]
    , [binaryStr "==" EEqual]
    ]

opChar :: Parser Char
opChar = oneOf chars  where
    chars :: [Char]
    chars = "!%&*+/<=>?@^-~"

binaryCh :: Char -> (a -> a -> a) -> Operator Parser a
binaryCh name f = InfixL (f <$ opCh name)

binaryStr :: Text -> (a -> a -> a) -> Operator Parser a
binaryStr name f = InfixL (f <$ opStr name)

binaryEmpty :: (a -> a -> a) -> Operator Parser a
binaryEmpty f = InfixL (pure f)

opCh :: Char -> Parser Char
opCh n = lexeme . try $ char n <* notFollowedBy opChar

opStr :: Text -> Parser Text
opStr n = lexeme . try $ string n <* notFollowedBy opChar

pExprVar :: Parser Expr
pExprVar = EVar <$> pVar

pVar :: Parser Text
pVar = noneOfTokens keywords *> pVar'
  where
    pVar' =
        ((<>) . T.singleton)
            <$> letterChar
            <*> (T.pack <$> lexeme (many alphaNumChar))
    noneOfTokens kw = notFollowedBy $ choice $ map pKeyword kw

pExprIf :: Parser Expr
pExprIf = do
    pKeyword "if"
    e <- pExpr
    pKeyword "then"
    e1 <- pExpr
    pKeyword "else"
    e2 <- pExpr
    return $ EIf e e1 e2

pExprLet :: Parser Expr
pExprLet = do
    pKeyword "let"
    x <- pVar
    pKeyword "be"
    e1 <- pExpr
    pKeyword "in"
    e2 <- pExpr
    return $ ELet e1 x e2

pExprLam :: Parser Expr
pExprLam = do
    charLexeme '\\'
    --charLexeme '('
    x <- pVar
    charLexeme ':'
    t <- pType
    --charLexeme ')'
    charLexeme '.'
    --charLexeme '('
    e <- pExpr
    --charLexeme ')'
    return $ ELam t x e

pType :: Parser Type
pType = makeExprParser typeTerm [[opTypeArrow]] <?> "type"

typeTerm :: Parser Type
typeTerm = choice
    [ brackets pType
    , TNum <$ pKeyword "num"
    , TStr <$ pKeyword "str"
    , TBool <$ pKeyword "bool"
    , TMeta <$> (char '$' *> pStringLiteral)
    ]

pExprMetaVar :: Parser Expr
pExprMetaVar = char '$' *> choice
    [ (\(e, t) -> EMeta e (Just t)) <$> brackets pVarAndType
    , EMeta <$> pStringLiteral <*> pure Nothing
    ]
  where
    pVarAndType :: Parser (Text, Type)
    pVarAndType = liftA2 (,) pStringLiteral (charLexeme ':' *> pType)

opTypeArrow :: Operator Parser Type
opTypeArrow = InfixR (TArrow <$ opStr "->")
