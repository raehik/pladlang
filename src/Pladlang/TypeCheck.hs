{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Pladlang.TypeCheck where

import Pladlang.AST
import qualified Pladlang.Derivation.AST as DerivAST
import Control.Monad.Trans.Class
import Data.Functor.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map

data Err
    = ErrTypeCheckFailed
    | ErrUnsupportedExpression Text
    deriving (Show, Eq)

type Parser = ReaderT Env (Except Err)

data Env = Env {
    envBindings :: Map Text Type
} deriving (Show, Eq)

tshow :: Show a => a -> Text
tshow = T.pack . show

addTypeBinding :: Text -> Type -> Env -> Env
addTypeBinding v t env =
    let bindings = envBindings env
        bindings' = Map.insert v t bindings
    in  env {envBindings=bindings'}

initEnv :: Env
initEnv = Env {envBindings=Map.empty}

getTypeDerivation :: Expr -> Either Err (Maybe Type, DerivAST.Rule)
getTypeDerivation = runExcept . flip runReaderT initEnv . typeCheck

typeCheck :: Expr -> Parser (Maybe Type, DerivAST.Rule)
typeCheck expr = case expr of
    ETrue -> do
        bindings <- asks envBindings
        let sequent = formSequent bindings (exprToDerivExpr ETrue) (typeToDerivType TBool)
            rule = validRule "bool" [] sequent
        return (Just TBool, rule)
    EFalse -> do
        bindings <- asks envBindings
        let sequent = formSequent bindings (exprToDerivExpr EFalse) (typeToDerivType TBool)
            rule = validRule "bool" [] sequent
        return (Just TBool, rule)
    EVar v -> do
        bindings <- asks envBindings
        case Map.lookup v bindings of
            Just t ->
                let sequent = formSequent bindings (exprToDerivExpr expr) (typeToDerivType t)
                    rule = validRule "var" [] sequent
                in return (Just t, rule)
            Nothing ->
                let sequent = formSequent bindings (exprToDerivExpr expr) (DerivAST.Tau Nothing)
                    rule = invalidRule (DerivAST.TypeErrorUndefinedVariableUsed v) sequent
                in return (Nothing, rule)
    ENum _ -> do
        bindings <- asks envBindings
        let sequent = formSequent bindings (exprToDerivExpr expr) (typeToDerivType TNum)
            rule = validRule "num" [] sequent
        return (Just TNum, rule)
    EStr _ -> do
        bindings <- asks envBindings
        let sequent = formSequent bindings (exprToDerivExpr expr) (typeToDerivType TStr)
            rule = validRule "str" [] sequent
        return (Just TStr, rule)
    EPlus e1 e2 -> typeCheckNumNumNum "plus" e1 e2
    ETimes e1 e2 -> typeCheckNumNumNum "times" e1 e2
    ECat e1 e2 -> do
        bindings <- asks envBindings
        let sequent = formSequent bindings (exprToDerivExpr expr) (typeToDerivType TStr)
        (e1Typ, e1Rule) <- typeCheck e1
        case e1Typ of
            Just TStr -> do
                (e2Typ, e2Rule) <- typeCheck e2
                let rule = validRule "cat" [e1Rule, e2Rule] sequent
                    typ = case e2Typ of { Just TStr -> Just TStr; _ -> Nothing }
                return (typ, rule)
            _ ->
                let e2Sequent = formSequent bindings (exprToDerivExpr e2) (typeToDerivType TStr)
                    e2Rule = DerivAST.SequentOnly $ e2Sequent
                    rule = validRule "cat" [e1Rule, e2Rule] sequent
                in return (Nothing, rule)
    ELen e -> do
        (eTyp, eRule) <- typeCheck e
        bindings <- asks envBindings
        let sequent = formSequent bindings (exprToDerivExpr expr) (typeToDerivType TNum)
        case eTyp of
            Just TStr ->
                let rule = validRule "len" [eRule] sequent
                in return (Just TNum, rule)
            Nothing ->
                let rule = invalidRule DerivAST.TypeErrorAny sequent
                in return (Nothing, rule)
            Just t ->
                let rule = invalidRule (DerivAST.TypeErrorArgWrongType (typeToDerivType t)) sequent
                in return (Nothing, rule)
    ELet e1 v e2 -> do
        (e1Typ, e1Rule) <- typeCheck e1
        case e1Typ of
            Nothing -> do
                bindings <- asks envBindings
                let sequent = formSequent bindings (exprToDerivExpr expr) (DerivAST.Tau Nothing)
                    --rule = invalidRule (DerivAST.TypeErrorArgWrongType (DerivAST.Tau Nothing)) sequent
                    e2SequentContext = (DerivAST.Binding v (DerivAST.Tau Nothing):bindingsToDerivContext bindings)
                    e2Sequent = DerivAST.Sequent { DerivAST.sequentContext=e2SequentContext, DerivAST.sequentExpr=exprToDerivExpr e2, DerivAST.sequentType=DerivAST.Tau (Just 1) }
                    e2Rule = DerivAST.SequentOnly $ e2Sequent
                    rule = validRule "let" [e1Rule, e2Rule] sequent
                return (Nothing, rule)
            Just e1Typ' -> do
                (e2Typ, e2Rule) <- local (addTypeBinding v e1Typ') (typeCheck e2)
                case e2Typ of
                    Nothing -> do
                        bindings <- asks envBindings
                        let sequent = formSequent bindings (exprToDerivExpr expr) (DerivAST.Tau Nothing)
                            rule = validRule "let" [e1Rule, e2Rule] sequent
                        return (Nothing, rule)
                    Just e2Typ' -> do
                        bindings <- asks envBindings
                        let sequent = formSequent bindings (exprToDerivExpr expr) (typeToDerivType e2Typ')
                            rule = validRule "let" [e1Rule, e2Rule] sequent
                        return (Just e2Typ', rule)
    ELam t v e -> do
        bindings <- asks envBindings
        let derivTypePart = DerivAST.TArrow (typeToDerivType t)
            untypedSequent = formSequent bindings (exprToDerivExpr expr)
        (eTyp, eRule) <- local (addTypeBinding v t) (typeCheck e)
        case eTyp of
            Nothing ->
                let sequent = untypedSequent $ derivTypePart (DerivAST.Tau Nothing)
                    rule = validRule "lam" [eRule] sequent
                in return (Nothing, rule)
            Just eTyp' ->
                let sequent = untypedSequent $ derivTypePart (typeToDerivType eTyp')
                    rule = validRule "lam" [eRule] sequent
                in return (Just (TArrow t eTyp'), rule)
    EAp e1 e2 -> do
        bindings <- asks envBindings
        let untypedSequent = formSequent bindings (exprToDerivExpr expr)
        (e1Typ, e1Rule) <- typeCheck e1
        case e1Typ of
            Nothing ->
                let sequent = untypedSequent $ DerivAST.Tau Nothing
                    e2Rule = DerivAST.SequentOnly $ formSequent bindings (exprToDerivExpr e2) (DerivAST.Tau (Just 1))
                    rule = validRule "ap" [e1Rule, e2Rule] sequent
                in return (Nothing, rule)
            Just (TArrow tArg tBody) -> do
                (e2Typ, e2Rule) <- typeCheck e2
                let sequent = untypedSequent $ typeToDerivType tBody
                    rule = validRule "ap" [e1Rule, e2Rule] sequent
                case e2Typ of
                    Nothing ->
                        return (Nothing, rule)
                    Just e2Typ' ->
                        if e2Typ' == tArg then
                            return (Just tBody, rule)
                        else
                            return (Nothing, rule)
            Just _ ->
                let sequent = untypedSequent $ DerivAST.Tau Nothing
                    e2Rule = DerivAST.SequentOnly $ formSequent bindings (exprToDerivExpr e2) (DerivAST.Tau (Just 1))
                    rule = validRule "ap" [e1Rule, e2Rule] sequent
                in return (Nothing, rule)
    e -> throwErr $ ErrUnsupportedExpression (tshow e)

typeCheckNumNumNum :: Text -> Expr -> Expr -> Parser (Maybe Type, DerivAST.Rule)
typeCheckNumNumNum name e1 e2 = do
    bindings <- asks envBindings
    let derivExpr = derivExprFunc name [e1, e2]
    let sequent = formSequent bindings derivExpr (typeToDerivType TNum)
    (e1typ, e1rule) <- typeCheck e1
    case e1typ of
        Just TNum -> do
            (e2typ, e2rule) <- typeCheck e2
            case e2typ of
                Just TNum ->
                    let rule = validRule name [e1rule, e2rule] sequent
                    in return (Just TNum, rule)
                Just _ ->
                    let rule = validRule name [e1rule, e2rule] sequent
                    in return (Nothing, rule)
                Nothing ->
                    --let rule = invalidRule (DerivAST.TypeErrorArgWrongType (DerivAST.Tau Nothing)) sequent
                    let rule = validRule name [e1rule, e2rule] sequent
                    in return (Nothing, rule)
        Just _ ->
            let rule = validRule name [e1rule] sequent
            in return (Just TNum, rule)
        Nothing ->
            --let rule = invalidRule (DerivAST.TypeErrorArgWrongType (DerivAST.Tau Nothing)) sequent
            let rule = validRule name [e1rule] sequent
            in return (Nothing, rule)

derivExprFunc :: Text -> [Expr] -> DerivAST.Expr
derivExprFunc name es = DerivAST.EFunc name (map exprToDerivExpr es)

throwErr :: Err -> Parser a
throwErr err = lift $ throwE err

exprToDerivExpr :: Expr -> DerivAST.Expr
exprToDerivExpr expr =
    case expr of
        EVar v -> DerivAST.EVar v
        ENum x -> DerivAST.ENum x
        EStr x -> DerivAST.EStr x
        EPlus e1 e2 -> derivExprFunc "plus" [e1, e2]
        ETimes e1 e2 -> derivExprFunc "times" [e1, e2]
        ECat e1 e2 -> derivExprFunc "cat" [e1, e2]
        ELen e ->
            DerivAST.EFunc "len" [exprToDerivExpr e]
        ELet e1 v e2 ->
            DerivAST.ELet (exprToDerivExpr e1) v (exprToDerivExpr e2)
        ELam t v e ->
            DerivAST.ELam (typeToDerivType t) v (exprToDerivExpr e)
        EAp e1 e2 ->
            DerivAST.EFunc "ap" [exprToDerivExpr e1, exprToDerivExpr e2]
        _ -> DerivAST.E Nothing

validRule :: Text -> [DerivAST.Rule] -> DerivAST.Sequent -> DerivAST.Rule
validRule name premises judgement =
    DerivAST.ValidRule DerivAST.ValidRule' {
        DerivAST.validRuleName=name,
        DerivAST.validRulePremises=premises,
        DerivAST.validRuleJudgement=judgement
    }

invalidRule :: DerivAST.TypeError -> DerivAST.Sequent -> DerivAST.Rule
invalidRule err judgement =
    DerivAST.InvalidRule DerivAST.InvalidRule' {
        DerivAST.invalidRuleError=err,
        DerivAST.invalidRuleJudgement=judgement
    }

formSequent
    :: Map Text Type -> DerivAST.Expr -> DerivAST.Type -> DerivAST.Sequent
formSequent bindings expr typ =
    let context = bindingsToDerivContext bindings in
    DerivAST.Sequent {
        DerivAST.sequentContext=context,
        DerivAST.sequentExpr=expr,
        DerivAST.sequentType=typ
    }

bindingsToDerivContext :: Map Text Type -> [DerivAST.ContextPart]
bindingsToDerivContext bs =
    if Map.null bs then
        []
    else
        map (\(v, t) -> DerivAST.Binding v (typeToDerivType t)) (Map.assocs bs)

typeToDerivType :: Type -> DerivAST.Type
typeToDerivType TNum = DerivAST.TNum
typeToDerivType TStr = DerivAST.TStr
typeToDerivType TBool = DerivAST.TBool
typeToDerivType (TArrow t1 t2) =
    DerivAST.TArrow (typeToDerivType t1) (typeToDerivType t2)

ruleUnsupported :: DerivAST.Rule
ruleUnsupported =
    DerivAST.InvalidRule DerivAST.InvalidRule' {
        DerivAST.invalidRuleError=DerivAST.TypeErrorAny,
        DerivAST.invalidRuleJudgement=sequentEmpty
    }

sequentEmpty :: DerivAST.Sequent
sequentEmpty =
    DerivAST.Sequent {
        DerivAST.sequentContext=[],
        DerivAST.sequentExpr=DerivAST.EVar "todo",
        DerivAST.sequentType=DerivAST.Tau Nothing
    }
