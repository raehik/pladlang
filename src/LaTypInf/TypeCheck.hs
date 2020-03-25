{-# LANGUAGE OverloadedStrings #-}

module LaTypInf.TypeCheck where

import LaTypInf.Types
import qualified LaTypInf.Derivation.AST as DerivAST
import Control.Monad.Trans.Class
import Data.Functor.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map

tshow :: Show a => a -> Text
tshow = T.pack . show

addTypeBinding v t env =
    let bindings = envBindings env in
    let bindings' = Map.insert v t bindings in
    env {envBindings=bindings'}

changeExpr e env = env {envExpr=e}

initEnv e = Env {envBindings=Map.empty, envExpr=e}

getTypeDerivation :: Expr -> Either Err (Maybe Type, DerivAST.Rule)
getTypeDerivation e =
    runIdentity $ runExceptT $ runReaderT typeCheck $ initEnv e

typeCheck = do
    env <- ask
    let expr = envExpr env
    case expr of
        EVar v ->
            case Map.lookup v (envBindings env) of
                Just t ->
                    let sequent = formSequent (envBindings env) (exprToDerivExpr expr) (typeToDerivType t)
                        rule = validRule "var" [] sequent
                    in return (Just t, rule)
                Nothing ->
                    let sequent = formSequent (envBindings env) (exprToDerivExpr expr) (DerivAST.Tau Nothing)
                        rule = invalidRule (DerivAST.TypeErrorUndefinedVariableUsed v) sequent
                    in return (Nothing, rule)
        ENum x ->
            let sequent = formSequent (envBindings env) (exprToDerivExpr expr) (typeToDerivType TNum)
                rule = validRule "num" [] sequent
            in return (Just TNum, rule)
        EStr x ->
            let sequent = formSequent (envBindings env) (exprToDerivExpr expr) (typeToDerivType TStr)
                rule = validRule "str" [] sequent
            in return (Just TStr, rule)
        EPlus e1 e2 -> do
            let sequent = formSequent (envBindings env) (exprToDerivExpr expr) (typeToDerivType TNum)
            (e1typ, e1rule) <- local (changeExpr e1) typeCheck
            case e1typ of
                Just TNum -> do
                    (e2typ, e2rule) <- local (changeExpr e2) typeCheck
                    case e2typ of
                        Just TNum ->
                            let rule = validRule "plus" [e1rule, e2rule] sequent
                            in return (Just TNum, rule)
                        Just typ ->
                            let rule = validRule "plus" [e1rule, e2rule] sequent
                            in return (Just TNum, rule)
                        Nothing ->
                            --let rule = invalidRule (DerivAST.TypeErrorArgWrongType (DerivAST.Tau Nothing)) sequent
                            let rule = validRule "plus" [e1rule, e2rule] sequent
                            in return (Just TNum, rule)
                Just typ ->
                    let rule = validRule "plus" [e1rule] sequent
                    in return (Just TNum, rule)
                Nothing ->
                    --let rule = invalidRule (DerivAST.TypeErrorArgWrongType (DerivAST.Tau Nothing)) sequent
                    let rule = validRule "plus" [e1rule] sequent
                    in return (Nothing, rule)
        ELet e1 v e2 -> do
            (e1typ, e1rule) <- local (changeExpr e1) typeCheck
            case e1typ of
                Nothing ->
                    let sequent = formSequent (envBindings env) (exprToDerivExpr expr) (DerivAST.Tau Nothing)
                        rule = invalidRule (DerivAST.TypeErrorArgWrongType (DerivAST.Tau Nothing)) sequent
                    in return (Nothing, rule)
                Just e1typ' -> do
                    (e2typ, e2rule) <- local (changeExpr e2 . addTypeBinding v e1typ') typeCheck
                    case e2typ of
                        Nothing ->
                            let sequent = formSequent (envBindings env) (exprToDerivExpr expr) (DerivAST.Tau Nothing)
                                rule = validRule "let" [e1rule, e2rule] sequent
                            in return (Nothing, rule)
                        Just e2typ' ->
                            let sequent = formSequent (envBindings env) (exprToDerivExpr expr) (typeToDerivType e2typ')
                                rule = validRule "let" [e1rule, e2rule] sequent
                            in return (Just e2typ', rule)
        _ -> throwErr ErrUnsupportedExpression

throwErr err = lift $ throwE err

exprToDerivExpr e =
    case e of
        EVar v -> DerivAST.EVar v
        ENum x -> DerivAST.ENum x
        EStr x -> DerivAST.EStr x
        EPlus e1 e2 ->
            DerivAST.EFunc "plus" [exprToDerivExpr e1, exprToDerivExpr e2]
        ELet e1 v e2 ->
            DerivAST.ELet (exprToDerivExpr e1) v (exprToDerivExpr e2)

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
            DerivAST.sequentType=typ
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
