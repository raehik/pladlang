{-# LANGUAGE OverloadedStrings #-}

module LaTypInf.Derivation.Examples where

import LaTypInf.Derivation.AST
import qualified Data.Text as T

-- Convenience print function.
p = putStrLn . T.unpack

--------------------------------------------------------------------------------

test1 = ValidRule ValidRule' {validRuleName="var", validRulePremises=[], validRuleJudgement=Sequent {sequentContext=[Binding "x" (Tau Nothing)], sequentExpr=EVar "x", sequentType=Tau Nothing}}

exBindPlus = ValidRule ValidRule' {
    validRuleName="let",
    validRulePremises=[
        ValidRule ValidRule' {
            validRuleName="num",
            validRulePremises=[],
            validRuleJudgement=
                Sequent {
                    sequentContext=[],
                    sequentExpr=ENum 1,
                    sequentType=TNum
                }
        },
        ValidRule ValidRule' {
            validRuleName="plus",
            validRulePremises=[
                ValidRule ValidRule' {
                    validRuleName="var",
                    validRulePremises=[],
                    validRuleJudgement=
                        Sequent {
                            sequentContext=[Binding "x" TNum],
                            sequentExpr=EVar "x",
                            sequentType=TNum
                        }
                },
                ValidRule ValidRule' {
                    validRuleName="num",
                    validRulePremises=[],
                    validRuleJudgement=
                        Sequent {
                            sequentContext=[Binding "x" TNum],
                            sequentExpr=ENum 2,
                            sequentType=TNum
                        }
                }
            ],
            validRuleJudgement=
                Sequent {
                    sequentContext=[Binding "x" TNum],
                    sequentExpr=EFunc "plus" [EVar "x", ENum 2],
                    sequentType=TNum
                }
        }
    ],
    validRuleJudgement=
        Sequent {
            sequentContext=[],
            sequentExpr=ELet (ENum 1) "x" (EFunc "plus" [EVar "x", ENum 2]),
            sequentType=TNum
        }
    }

exQuickFail =
    ValidRule ValidRule' {
        validRuleName="plus",
        validRulePremises=[
            InvalidRule InvalidRule' {
                invalidRuleError=TypeErrorUndefinedVariableUsed "x",
                invalidRuleJudgement=
                    Sequent {
                        sequentContext=[],
                        sequentExpr=EVar "x",
                        sequentType=TNum
                    }
            },
            ValidRule ValidRule' {
                validRuleName="num",
                validRulePremises=[],
                validRuleJudgement=
                    Sequent {
                        sequentContext=[],
                        sequentExpr=ENum 2,
                        sequentType=TNum
                    }
            }
        ],
        validRuleJudgement=
            Sequent {
                sequentContext=[],
                sequentExpr=EFunc "plus" [EVar "x", ENum 2],
                sequentType=TNum
            }
    }
