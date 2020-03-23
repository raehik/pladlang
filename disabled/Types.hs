{-# LANGUAGE OverloadedStrings #-}

module LaTypInf.Types where

import qualified LaTypInf.Derivation.AST as DerivAST
import Control.Monad.Trans.Class
import Data.Functor.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

data Err
    = ErrVariableRebound
    | ErrUndefinedVariableUsed
    | ErrUnsupported
    deriving (Show)

type FailedTypeCheck = (Err, DerivAST.Rule)

type Parser = ReaderT Env (ExceptT FailedTypeCheck Identity)

data Env = Env {
    envBindings :: Map VVar Type,
    envExpr :: Expr
} deriving (Show)

type VVar = Text
type VNum = Int
type VStr = Text
data VBool = VTrue | VFalse deriving (Show)

data Type
    = TNum
    | TStr
    | TBool
    | TArrow Type Type
    deriving (Show)

data Expr
    = EVar VVar
    | ENum VNum
    | EPlus Expr Expr
    | ELet Expr VVar Expr
    deriving (Show)

addTypeBinding v t env =
    let bindings = envBindings env in
    let bindings' = Map.insert v t bindings in
    env {envBindings=bindings'}

initEnv e = Env {envBindings=Map.empty, envExpr=e}
runTypeCheck e = runIdentity $ runExceptT $ runReaderT typeCheck $ initEnv e
f1 = runTypeCheck $ ENum 1
f2 = runTypeCheck $ ELet (ENum 1) "x" (EPlus (ENum 1) (EVar "x"))
f3 = runTypeCheck $ ELet (ENum 1) "x" (EPlus (ENum 1) (EVar "y"))

typeCheck = do
    env <- ask
    let expr = envExpr env
    case expr of
        _ -> throwE (ErrUnsupported, ruleUnsupported)

ruleUnsupported =
    DerivAST.Rule {
        ruleName="todo",
        rulePremises=Left DerivAST.TypeCheckFailureAny,
        ruleJudgement=sequentEmpty
    }

sequentEmpty =
    DerivAST.Sequent {
        sequentContext=[],
        sequentExpr=DerivAST.EVar "todo",
        sequentType=DerivAST.Tau
    }
