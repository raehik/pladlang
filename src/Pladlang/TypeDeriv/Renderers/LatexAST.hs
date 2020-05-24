{-# LANGUAGE OverloadedStrings #-}

module Pladlang.TypeDeriv.Renderers.LatexAST where

import Pladlang.AST
import TypeDeriv.AST
import qualified TypeDeriv.Renderer.Latex as TD
import Data.Text (Text)
import qualified Data.Text as T

-- | Render a Pladlang type derivation to LaTeX, displaying expressions
-- according to the Pladlang AST.
render :: Rule Expr Type -> Text
render = TD.render TD.defCfg renderExpr renderType

renderExpr :: TD.Config -> Expr -> Text
renderExpr _   (EMeta anno)   =
    if anno == T.empty
    then "e"
    else "e_{" <> anno <> "}"
renderExpr _   (EVar v)       = "\\textcolor[rgb]{0.0, 0.5, 1.0}{" <> v <> "}"
renderExpr cfg (ENum x)       = renderType cfg TNum <> "[" <> tshow x <> "]"
renderExpr cfg (EStr x)       = renderType cfg TStr <> "[" <> x <> "]"
renderExpr _    ETrue         = mathtt "true"
renderExpr _    EFalse        = mathtt "false"
renderExpr cfg (EPlus e1 e2)  = renderFunc cfg "plus"  [e1, e2]
renderExpr cfg (ETimes e1 e2) = renderFunc cfg "times" [e1, e2]
renderExpr cfg (ECat e1 e2)   = renderFunc cfg "cat"   [e1, e2]
renderExpr cfg (EEqual e1 e2) = renderFunc cfg "equal" [e1, e2]
renderExpr cfg (EAp e1 e2)    = renderFunc cfg "ap"    [e1, e2]
renderExpr cfg (EIf e el er)  = renderFunc cfg "if"    [e, el, er]
renderExpr cfg (ELen e)       = mathtt "len" <> "(" <> renderExpr cfg e <> ")"
renderExpr cfg (ELet e1 v e2) =
    mathtt "let"
    <> "("   <> renderExpr cfg e1
    <> " ; " <> renderExpr cfg (EVar v)
    <> " . " <> renderExpr cfg e2
    <> ")"
renderExpr cfg (ELam t v e) =
    mathtt "lam"
    <> "\\{" <> renderType cfg t <> "\\}"
    <> "("   <> renderExpr cfg (EVar v)
    <> " . " <> renderExpr cfg e <> ")"

renderFunc :: TD.Config -> Text -> [Expr] -> Text
renderFunc cfg func es =
    mathtt func <> "("
    <> T.intercalate " ; " (map (renderExpr cfg) es)
    <> ")"

mathtt :: Text -> Text
mathtt x = "\\mathtt{" <> x <> "}"

renderType :: TD.Config -> Type -> Text
renderType _   (TMeta anno)   =
    if anno == T.empty
    then "\\tau"
    else "\\tau_{" <> anno <> "}"
renderType _    TNum          = mathtt "num"
renderType _    TStr          = mathtt "str"
renderType _    TBool         = mathtt "bool"
renderType cfg (TArrow t1 t2) =
    mathtt "arr"
    <> "("   <> renderType cfg t1
    <> " ; " <> renderType cfg t2
    <> ")"

tshow :: Show a => a -> Text
tshow = T.pack . show
