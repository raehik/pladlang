{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module LaTypInf.Types where

import Control.Monad.Trans.Class
import Data.Functor.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

tshow :: Show a => a -> Text
tshow = T.pack . show

data Err
    = ErrVariableRebound
    | ErrUndefinedVariableUsed
    deriving (Show)

type VVar = Text
type VNum = Int
type VStr = Text
data VBool = VTrue | VFalse deriving (Show)

type Parser = ReaderT Env (ExceptT Err Identity)

data Env = Env {
    envTypeBindings :: Map VVar Type,
    envExpr :: Expr
} deriving (Show)

class ShowAbstractSyntax a where
    showAbstractSyntax :: a -> Parser Text

class (ShowAbstractSyntax a) => IsExpr a where
    typeOf :: a -> Parser Type
    showTypeInference :: a -> Parser Text
    showTypeInference e = do
        t <- typeOf e
        asE <- showAbstractSyntax e
        asT <- showAbstractSyntax t
        return $ asE <> " : " <> asT

data Type
    = TNum
    | TStr
    | TBool
    | TArrow Type Type
    deriving (Show)
instance ShowAbstractSyntax Type where
    showAbstractSyntax TNum = return "\\mathtt{num}"
    showAbstractSyntax TStr = return "\\mathtt{str}"
    showAbstractSyntax TBool = return "\\mathtt{bool}"
    showAbstractSyntax (TArrow t1 t2) = do
        asT1 <- showAbstractSyntax t1
        asT2 <- showAbstractSyntax t2
        return $ "\\mathtt{arr} (" <> asT1 <> " ; " <> asT2 <> ")"

data Expr
    = EVar VVar
    | ENum VNum
    | EPlus Expr Expr
    | ELet Expr VVar Expr
    deriving (Show)

instance ShowAbstractSyntax Expr where
    showAbstractSyntax (EVar x) = return $ "\\var{" <> x <> "}"
    showAbstractSyntax (ENum x) = return $ "\\mathtt{num}[" <> tshow x <> "]"
    showAbstractSyntax (EPlus e1 e2) = do
        asE1 <- showAbstractSyntax e1
        asE2 <- showAbstractSyntax e2
        return $ "\\mathtt{plus} (" <> asE1 <> " ; " <> asE2 <> ")"
    showAbstractSyntax (ELet e1 x e2) = do
        asE1 <- showAbstractSyntax e1
        asE2 <- showAbstractSyntax e2
        return $ "\\mathtt{let} (" <> asE1 <> " ; " <> x <> " . " <> asE2 <> ")"

instance IsExpr Expr where
    typeOf (EVar x) = do
        env <- ask
        case Map.lookup x (envTypeBindings env) of
            Nothing -> lift $ throwE $ ErrUndefinedVariableUsed
            Just v -> return $ v
    typeOf (ENum _) = return $ TNum
    typeOf (EPlus _ _) = return $ TArrow TNum TNum
    typeOf (ELet e1 x e2) = typeOf e2
    showTypeInference (EPlus e1 e2) = do
        asE <- showAbstractSyntax (EPlus e1 e2)
        t <- typeOf (EPlus e1 e2)
        asT <- showAbstractSyntax t
        upE1 <- showTypeInference e1
        upE2 <- showTypeInference e2
        return $
            "\\inferrule*[Left=\\textbf{EF}-ty-plus]"
            <> "{" <> upE1 <> " \\\\ " <> upE2 <> "}"
            <> "{" <> asE <> " : " <> asT <> "}"
    showTypeInference (ELet e1 x e2) = do
        env <- ask
        if Map.member x (envTypeBindings env) then
            lift $ throwE ErrVariableRebound
        else do
            tE1 <- typeOf e1
            upE1 <- local (addTypeBinding x tE1) (showTypeInference e1)
            upE2 <- local (addTypeBinding x tE1) (showTypeInference e2)
            asE <- showAbstractSyntax (ELet e1 x e2)
            t <- typeOf (ELet e1 x e2)
            asT <- showAbstractSyntax t
            return $
                "\\inferrule*[Left=\\textbf{EF}-ty-let]"
                <> "{" <> upE1 <> " \\\\ " <> upE2 <> "}"
                <> "{" <> asE <> " : " <> asT <> "}"
    showTypeInference e = do
        t <- typeOf e
        asE <- showAbstractSyntax e
        asT <- showAbstractSyntax t
        return $ asE <> " : " <> asT

addTypeBinding v t env =
    let bindings = envTypeBindings env in
    let bindings' = Map.insert v t bindings in
    env {envTypeBindings=bindings'}

initEnv = Env {envTypeBindings=Map.empty, envExpr=(ENum 0)}
f e = runIdentity $ runExceptT $ runReaderT (showTypeInference e) $ initEnv
f1 = f $ ENum 1
f2 = f $ ELet (ENum 1) "x" (EPlus (ENum 1) (EVar "x"))
f3 = f $ ELet (ENum 1) "x" (EPlus (ENum 1) (EVar "y"))
