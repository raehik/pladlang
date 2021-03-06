-- | The Pladlang AST.
--
-- A simple expression tree.
--
-- Note that lambda expressions take the type of the variable. Otherwise, type
-- information is separate to the AST.
module Pladlang.AST where

import           Data.Text                      ( Text )

type Var = Text

data Expr
    -- | Annotated expression metavar.
    = EMeta Text

    | EVar Var
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
    | ELet Expr Var Expr
    | ELam Type Text Expr
    | EAp Expr Expr
    deriving (Eq, Show)

data Type
    -- | Annotated type metavar.
    = TMeta Text

    -- | Arrow type (function).
    | TArrow Type Type

    | TNum
    | TStr
    | TBool
    deriving (Eq, Show)

{-
data TopExpr
    = TopExpr Expr [ContextPart]
    deriving (Eq, Show)

data ContextPart
    -- | Regular context binding.
    = CtxBinding Var Type

    -- | Annotated context metavar.
    | CtxMeta Text
    deriving (Eq, Show)
-}
