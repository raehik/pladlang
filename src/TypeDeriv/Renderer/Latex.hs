{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TypeDeriv.Renderer.Latex where

import TypeDeriv.AST
import Data.Text (Text)
import qualified Data.Text as T

data Config = Config
    { cfgUseAmssymbRounderSetSymbol :: Bool
    } deriving (Eq, Show)

defCfg :: Config
defCfg = Config { cfgUseAmssymbRounderSetSymbol = True }

render
    :: Config
    -> (Config -> e -> Text)
    -> (Config -> t -> Text)
    -> Rule e t
    -> Text
render cfg renderExpr renderType rule =
    let rLabel =
            if T.null (ruleLabel rule)
            then T.empty
            else "[Left=" <> ruleLabel rule <> "]"
        rPremises = renderPremises (rulePremises rule)
        rJudgement =
            renderContext (sequentContext judge)
            <> " \\vdash " <> renderExpr cfg (sequentExpr judge)
            <> " : "       <> renderType cfg (sequentType judge)
        judge = ruleJudgement rule
    in
        "\\inferrule*"
        <> rLabel
        <> "{" <> rPremises  <> "}"
        <> "{" <> rJudgement <> "}"
  where
    renderPremises [] = " "
    renderPremises ps =
        T.intercalate " \\\\ "
        . map (render cfg renderExpr renderType)
        $ ps
    renderContext [] =
        if cfgUseAmssymbRounderSetSymbol cfg
        then "\\varnothing"
        else "\\emptyset"
    renderContext parts = T.intercalate ", " (map renderContextPart parts)
    renderContextPart =
        \case
            CtxBinding v t ->
                v <> " : " <> renderType cfg t
            CtxMeta anno ->
                if T.null anno
                then "\\Gamma"
                else "\\Gamma_{" <> anno <> "}"

-- | Render any showable expression to LaTeX with default options. Expressions
-- and types are wrapped in mathtt. Intended for debugging purposes.
render' :: (Show e, Show t) => Rule e t -> Text
render' = render defCfg (flip . const $ ttshow) (flip . const $ ttshow)
  where
    ttshow x = "\\mathtt{" <> (T.pack . show) x <> "}"
