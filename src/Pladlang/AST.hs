-- | The Pladlang AST.
--
-- A simple tree of expressions.
--
-- Note that lambda expressions take the type of the variable. Otherwise, type
-- information is separate to the AST.
module Pladlang.AST where

import Data.Text (Text)

data Type
    = TNum
    | TStr
    | TBool
    | TArrow Type Type
    deriving (Show, Eq)

data Expr
    = EVar Text
    | ENum Integer
    | EStr Text
    | ETrue
    | EFalse
    | EPlus Expr Expr
    | ETimes Expr Expr
    | ECat Expr Expr
    | ELen Expr
    | EEqual Expr Expr
    | EIf Expr Expr Expr
    | ELet Expr Text Expr
    | ELam Type Text Expr
    | EAp Expr Expr
    deriving (Show, Eq)
