{-# LANGUAGE OverloadedStrings #-}

module LaTypInf.Derivation.Examples where

import LaTypInf.Derivation.AST
import qualified Data.Text as T

-- Convenience print function.
p = putStrLn . T.unpack

--------------------------------------------------------------------------------

test1 = Rule {ruleName=Just "var", rulePremises=Right [], ruleJudgement=Sequent {sequentContext=[Binding "x" (Tau Nothing)], sequentExpr=EVar "x", sequentType=Tau Nothing}}

exBindPlus = Rule {
    ruleName=Just "let",
    rulePremises=Right [
        Rule {
            ruleName=Just "num",
            rulePremises=Right [],
            ruleJudgement=
                Sequent {
                    sequentContext=[],
                    sequentExpr=ENum 1,
                    sequentType=TNum
                }
        },
        Rule {
            ruleName=Just "plus",
            rulePremises=Right [
                Rule {
                    ruleName=Just "var",
                    rulePremises=Right [],
                    ruleJudgement=
                        Sequent {
                            sequentContext=[Binding "x" TNum],
                            sequentExpr=EVar "x",
                            sequentType=TNum
                        }
                },
                Rule {
                    ruleName=Just "num",
                    rulePremises=Right [],
                    ruleJudgement=
                        Sequent {
                            sequentContext=[Binding "x" TNum],
                            sequentExpr=ENum 2,
                            sequentType=TNum
                        }
                }
            ],
            ruleJudgement=
                Sequent {
                    sequentContext=[Binding "x" TNum],
                    sequentExpr=EFunc "plus" [EVar "x", ENum 2],
                    sequentType=TNum
                }
        }
    ],
    ruleJudgement=
        Sequent {
            sequentContext=[],
            sequentExpr=ELet (ENum 1) "x" (EFunc "plus" [EVar "x", ENum 2]),
            sequentType=TNum
        }
    }

exQuickFail =
    Rule {
        ruleName=Just "plus",
        rulePremises=Right [
            Rule {
                ruleName=Nothing,
                rulePremises=Left (TypeErrorUndefinedVariableUsed "x"),
                ruleJudgement=
                    Sequent {
                        sequentContext=[],
                        sequentExpr=EVar "x",
                        sequentType=TNum
                    }
            },
            Rule {
                ruleName=Just "num",
                rulePremises=Right [],
                ruleJudgement=
                    Sequent {
                        sequentContext=[],
                        sequentExpr=ENum 2,
                        sequentType=TNum
                    }
            }
        ],
        ruleJudgement=
            Sequent {
                sequentContext=[],
                sequentExpr=EFunc "plus" [EVar "x", ENum 2],
                sequentType=TNum
            }
    }
