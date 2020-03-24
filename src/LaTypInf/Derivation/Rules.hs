{-# LANGUAGE OverloadedStrings #-}

module LaTypInf.Derivation.Rules where

import LaTypInf.Derivation.AST

trVar = ValidRule {
    validRuleName="var",
    validRulePremises=[],
    validRuleJudgement=
        Sequent {
            sequentContext=[
                Gamma (Just 1),
                Binding "x" (Tau Nothing),
                Gamma (Just 2)
            ],
            sequentExpr=EVar "x",
            sequentType=Tau Nothing
        }
    }
