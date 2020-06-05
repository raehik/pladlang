{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module HMInfer.Types where

import Data.Text (Text)

type Var = Text

data Judgement = Judgement
    { judgementContext :: [ContextBinding Var t]
    , judgementExpr :: e
    , judgementType :: t
    } deriving (Eq, Show)

data Rule e t = Rule
    { ruleLabel :: Text
    , rulePremises :: [Judgement e t]
    , ruleConclusion :: Judgement e t
    } deriving (Eq, Show)

data Derivation e t = Derivation
    { derivLabel :: Text
    , derivPremises :: [Rule e t]
    , derivConclusion :: Judgement e t
    } deriving (Eq, Show)
