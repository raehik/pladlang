{-# LANGUAGE OverloadedStrings #-}

module LaTypInf.Derivation.Examples where

import LaTypInf.Derivation.AST
import Data.Text (Text)
import qualified Data.Text as T

-- Show -> Text.
tshow :: Show a => a -> Text
tshow = T.pack . show

-- Wrap a string in mathtt.
tt :: Text -> Text
tt t = "\\mathtt{" <> t <> "}"

--------------------------------------------------------------------------------

-- Print.
p = putStrLn . T.unpack

test1 = Rule {ruleName="var", rulePremises=[], ruleJudgement=Sequent {sequentContext=[Binding "x" (Tau Nothing)], sequentExpr=EVar "x", sequentType=Tau Nothing}}
--test2 = Rule {ruleName="let", rulePremises=[test1], ruleJudgement=Sequent {sequentContext=NullSet, sequentExpr=ELet (ENum 1) "x" (EVar "x"), sequentType=Tau Nothing}}

trVar = Rule {
    ruleName="var",
    rulePremises=[],
    ruleJudgement=
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
