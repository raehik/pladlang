{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Pladlang.TypeDeriv where

import Pladlang.AST
import TypeDeriv.AST
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map

type Bindings = Map Pladlang.AST.Var Type

bindingsToContext :: Bindings -> [ContextPart Type]
bindingsToContext bs = Map.foldrWithKey (\v t b -> CtxBinding v t : b) [] bs

typeDerive :: Bindings -> Expr -> Rule Expr Type
typeDerive bs e = case e of
    ENum _ -> axiom "num" TNum
    EStr _ -> axiom "str" TStr
    EPlus e1 e2 -> ruleFunc "plus" TNum [e1, e2]

    _ -> Rule
        { ruleLabel = "ERR"
        , rulePremises = []
        , ruleJudgement = Sequent
            { sequentContext = []
            , sequentExpr = e
            , sequentType = TMeta T.empty
            }
        }
  where
    -- | A rule with no premises.
    axiom :: Text -> Type -> Rule Expr Type
    axiom name t = Rule
        { ruleLabel = name
        , rulePremises = []
        , ruleJudgement = Sequent
            { sequentContext = []
            , sequentExpr = e
            , sequentType = t
            }
        }

    -- A rule with a list of premises, which are all the same type as the rule.
    ruleFunc :: Text -> Type -> [Expr] -> Rule Expr Type
    ruleFunc name t es = TODO a fold

    EPlus e1 e2 -> Rule
        { ruleLabel = "plus"
        , rulePremises = [typeDerive bs e1, typeDerive bs e2]
        , ruleJudgement = Sequent
            { sequentContext = []
            , sequentExpr = e
            , sequentType = TNum
            }
        }
