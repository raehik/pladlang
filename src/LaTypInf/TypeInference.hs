module LaTypInf.TypeInference where

import Data.Text (Text)

data Rule = Rule {
    ruleName :: Text,
    ruleJudgement :: Judgement,
    ruleTopBit :: Text
} deriving (Show)

type Judgement = (Context, Expr, Type)

data Context
    = NullSet
    | Gamma
    deriving (Show)

data Expr
    = EVar Text
    | EFunc Text [Expr]
    deriving (Show)

data Type
    = TNum
    | TStr
    | TBool
    | TArrow Type Type
    deriving (Show)
