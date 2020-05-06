{-# LANGUAGE OverloadedStrings #-}

module Pladlang.Derivation.Rules where

import Pladlang.Derivation.AST

trVar = ValidRule ValidRule' {
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
