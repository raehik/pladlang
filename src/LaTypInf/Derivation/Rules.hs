{-# LANGUAGE OverloadedStrings #-}

module LaTypInf.Derivation.Rules where

import LaTypInf.Derivation.AST

--------------------------------------------------------------------------------

exBindPlus = Rule {
    ruleName="let",
    rulePremises=[
        Rule {
            ruleName="num",
            rulePremises=[],
            ruleJudgement=
                Sequent {
                    sequentContext=[],
                    sequentExpr=ENum 1,
                    sequentType=TNum
                }
        },
        Rule {
            ruleName="plus",
            rulePremises=[
                Rule {
                    ruleName="var",
                    rulePremises=[],
                    ruleJudgement=
                        Sequent {
                            sequentContext=[Binding "x" TNum],
                            sequentExpr=EVar "x",
                            sequentType=TNum
                        }
                },
                Rule {
                    ruleName="num",
                    rulePremises=[],
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
