{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module LaTypInf.Types where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Reader
import qualified Data.List as List

tshow :: Show a => a -> Text
tshow = T.pack . show

class ShowAbstractSyntax a where
    showAbstractSyntax :: a -> Text

class (ShowAbstractSyntax a) => IsExpr a where
    typeOf :: a -> Type
    --deriveTypeC :: a -> Reader [Binding] Text
    showExprAbstractSyntax :: a -> Text
    showExprAbstractSyntax x =
        showAbstractSyntax x <> " : " <> showAbstractSyntax (typeOf x)

data Type
    = TNum
    | TStr
    | TBool
    | TArrow Type Type
instance ShowAbstractSyntax Type where
    showAbstractSyntax TNum = "\\mathtt{num}"
    showAbstractSyntax TStr = "\\mathtt{str}"
    showAbstractSyntax TBool = "\\mathtt{bool}"
    showAbstractSyntax (TArrow t1 t2) =
        "\\mathtt{arr} ("
        <> showAbstractSyntax t1 <> " ; " <> showAbstractSyntax t2 <> ")"

-- TODO: better way of handling vars? And plain values in general, actually

data Expr a where
    EVar    :: VVar -> Type -> Expr VVar
    ENum    :: VNum -> Expr VNum
    EPlus   :: Expr VNum -> Expr VNum -> Expr VNum
instance ShowAbstractSyntax (Expr a) where
    showAbstractSyntax (EVar x _) = "\\var{" <> x <> "}"
    showAbstractSyntax (ENum x) = "\\mathtt{num}[" <> tshow x <> "]"
    showAbstractSyntax (EPlus e1 e2) =
        "\\mathtt{plus} ("
        <> showAbstractSyntax e1 <> " ; " <> showAbstractSyntax e2 <> ")"
instance IsExpr (Expr a) where
    typeOf (EVar _ t) = t
    typeOf (ENum _) = TNum
    typeOf (EPlus _ _) = TArrow TNum TNum

type Binding = (VVar, Type)

type VVar = Text
type VNum = Int
type VStr = Text
data VBool = VTrue | VFalse deriving (Show)
