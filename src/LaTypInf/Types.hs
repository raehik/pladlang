{-# LANGUAGE OverloadedStrings #-}

module LaTypInf.Types where

import qualified LaTypInf.Derivation.AST as DerivAST
import qualified LaTypInf.Derivation.Converter.Latex as DerivConvLatex
import Control.Monad.Trans.Class
import Data.Functor.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

-- Show -> Text.
tshow :: Show a => a -> Text
tshow = T.pack . show

tprint :: Text -> IO ()
tprint = putStrLn . T.unpack

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
    deriving (Show)

data Expr
    = EVar VVar
    | ENum VNum
    | EStr VStr
    | EPlus Expr Expr
    | ELet Expr VVar Expr
    deriving (Show)

addTypeBinding v t env =
    let bindings = envBindings env in
    let bindings' = Map.insert v t bindings in
    env {envBindings=bindings'}

changeExpr e env = env {envExpr=e}

initEnv e = Env {envBindings=Map.empty, envExpr=e}

getTypeDerivation :: Expr -> Text
getTypeDerivation e =
    case runIdentity $ runExceptT $ runReaderT typeCheck $ initEnv e of
        Left err -> tshow err
        Right (typ, rule) -> DerivConvLatex.showRule rule

f1 = getTypeDerivation $ ENum 1
f2 = getTypeDerivation $ ELet (ENum 1) "x" (EPlus (ENum 1) (EVar "x"))
f3 = getTypeDerivation $ ELet (ENum 1) "x" (EPlus (ENum 1) (EVar "y"))

typeCheck = do
    env <- ask
    let expr = envExpr env
    case expr of
        ENum x ->
            let sequent = formSequent (envBindings env) (DerivAST.ENum x) TNum
                rule = validRule "num" [] sequent
            in return (Just TNum, rule)
        EStr x ->
            let sequent = formSequent (envBindings env) (DerivAST.EStr x) TNum
                rule = validRule "str" [] sequent
            in return (Just TStr, rule)
        EPlus e1 e2 -> do
            let sequent = formSequent (envBindings env) (DerivAST.EFunc "plus" []) TNum
            (e1typ, e1rule) <- local (changeExpr e1) typeCheck
            case e1typ of
                Just TNum -> do
                    (e2typ, e2rule) <- local (changeExpr e2) typeCheck
                    case e2typ of
                        Just TNum ->
                            let rule = validRule "plus" [e1rule, e2rule] sequent
                            in return (Just TNum, rule)
                        Just typ ->
                            let rule = invalidRule (DerivAST.TypeErrorArgWrongType (typeToDerivType typ)) sequent
                            in return (Nothing, rule)
                        Nothing -> throwErr ErrTypeCheckFailed
                Just typ -> throwErr ErrTypeCheckFailed
                Nothing -> throwErr ErrTypeCheckFailed
        _ -> throwErr ErrUnsupportedExpression

throwErr err = lift $ throwE err

validRule name premises judgement =
    DerivAST.ValidRule DerivAST.ValidRule' {
        DerivAST.validRuleName=name,
        DerivAST.validRulePremises=premises,
        DerivAST.validRuleJudgement=judgement
    }

invalidRule err judgement =
    DerivAST.InvalidRule DerivAST.InvalidRule' {
        DerivAST.invalidRuleError=err,
        DerivAST.invalidRuleJudgement=judgement
    }

formSequent bindings expr typ =
    let
        context =
            if Map.null bindings then
                []
            else
                map (\(v, t) -> DerivAST.Binding v (typeToDerivType t)) (Map.assocs bindings)
    in
        DerivAST.Sequent {
            DerivAST.sequentContext=context,
            DerivAST.sequentExpr=expr,
            DerivAST.sequentType=typeToDerivType typ
        }

typeToDerivType :: Type -> DerivAST.Type
typeToDerivType TNum = DerivAST.TNum
typeToDerivType TStr = DerivAST.TStr
typeToDerivType TBool = DerivAST.TBool
typeToDerivType (TArrow t1 t2) =
    DerivAST.TArrow (typeToDerivType t1) (typeToDerivType t2)

ruleUnsupported =
    DerivAST.InvalidRule DerivAST.InvalidRule' {
        DerivAST.invalidRuleError=DerivAST.TypeErrorAny,
        DerivAST.invalidRuleJudgement=sequentEmpty
    }

sequentEmpty =
    DerivAST.Sequent {
        DerivAST.sequentContext=[],
        DerivAST.sequentExpr=DerivAST.EVar "todo",
        DerivAST.sequentType=DerivAST.Tau Nothing
    }
