{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TypeDeriv.AST where

import Data.Text (Text)

type Var = Text

data Rule e t = Rule
    { ruleLabel :: Text
    , rulePremises :: [Rule e t]
    , ruleJudgement :: Sequent e t
    } deriving (Eq, Show)

data Sequent e t = Sequent
    { sequentContext :: [ContextPart t]
    , sequentExpr :: e
    , sequentType :: t
    } deriving (Eq, Show)

data ContextPart t
    = CtxBinding Var t
    | CtxMeta Text
    deriving (Eq, Show)
