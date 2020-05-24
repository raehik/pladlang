-- | Render a Pladlang type derivation to LaTeX, displaying expressions
-- according to the Pladlang concrete syntax.
--
-- TODO: check spacing!

{-# LANGUAGE OverloadedStrings #-}

module Pladlang.TypeDeriv.Renderers.LatexCS where

import Pladlang.AST
import TypeDeriv.AST
import qualified TypeDeriv.Renderer.Latex as TD
import Data.Text (Text)
import qualified Data.Text as T

render :: Rule Expr Type -> Text
render = TD.render TD.defCfg renderExpr renderType

renderExpr :: TD.Config -> Expr -> Text
renderExpr _   (EMeta anno)   =
    if anno == T.empty
    then "e"
    else "e_{" <> anno <> "}"
renderExpr _   (EVar v)       = "\\textcolor[rgb]{0.0, 0.5, 1.0}{" <> v <> "}"
renderExpr _   (ENum x)       = tshow x
renderExpr _   (EStr x)       = "\"" <> x <> "\""
renderExpr _    ETrue         = mathtt "true"
renderExpr _    EFalse        = mathtt "false"
renderExpr cfg (ELen e)       = "|" <> renderExpr cfg e <> "|"
renderExpr cfg (EIf e el er)  =
    mathtt "if"
    <> renderExpr cfg e
    <> mathtt "then"
    <> renderExpr cfg el
    <> mathtt "else"
    <> renderExpr cfg er
renderExpr cfg (ELet e1 v e2) =
    mathtt "let"
    <> renderExpr cfg (EVar v)
    <> mathtt "be"
    <> renderExpr cfg e1
    <> mathtt "in"
    <> renderExpr cfg e2
renderExpr cfg (ELam t v e) =
    "\\lambda"
    <> renderExpr cfg (EVar v)
    <> " : " <> renderType cfg t
    <> " . " <> renderExpr cfg e
renderExpr _   _ = "(unsupported expression)"

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
renderType _    _ = "(unsupported type)"

tshow :: Show a => a -> Text
tshow = T.pack . show
