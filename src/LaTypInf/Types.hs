module LaTypInf.Types where

import Control.Monad.Trans.Class
import Data.Functor.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Data.Text (Text)
import Data.Map (Map)

data Err
    = ErrTypeCheckFailed
    | ErrUnsupportedExpression
    deriving (Show)

type Parser = ReaderT Env (ExceptT Err Identity)

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
    deriving (Show, Eq)

data Expr
    = EVar VVar
    | ENum VNum
    | EStr VStr
    | EPlus Expr Expr
    | ETimes Expr Expr
    | ELet Expr VVar Expr
    | ELam Type VVar Expr
    | EAp Expr Expr
    deriving (Show)
