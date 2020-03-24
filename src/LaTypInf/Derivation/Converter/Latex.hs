{-# LANGUAGE OverloadedStrings #-}

module LaTypInf.Derivation.Converter.Latex where

import LaTypInf.Derivation.AST
import Data.Text (Text)
import qualified Data.Text as T

-- Show -> Text.
tshow :: Show a => a -> Text
tshow = T.pack . show

-- Wrap a string in mathtt.
tt :: Text -> Text
tt t = "\\mathtt{" <> t <> "}"

showRule :: Rule -> Text
showRule (ValidRule r) =
    let name = showRuleName (validRuleName r)
        premises = showPremises (validRulePremises r)
        judgement = showSequent (validRuleJudgement r)
    in constructInferRule (Just name) premises judgement
showRule (InvalidRule r) =
    let premises = showTypeError (invalidRuleError r)
        judgement = showSequent (invalidRuleJudgement r)
    in constructInferRule Nothing premises judgement

constructInferRule :: Maybe Text -> Text -> Text -> Text
constructInferRule optName premises judgement =
    "\\inferrule*"
    <> case optName of { Nothing -> ""; Just name -> name }
    <> "{" <> premises <> "}"
    <> "{" <> judgement <> "}"

showRuleName :: Text -> Text
showRuleName name = "[Left=" <> name <> "]"

showPremises :: [Rule] -> Text
showPremises [] = " "
showPremises rs = T.intercalate " \\\\ " $ map showRule rs

showTypeError (TypeErrorUndefinedVariableUsed v) =
    "\\color{rred} " <> v <> " \\ " <> tt "undecl."
showTypeError (TypeErrorAny) = "type error"

showSequent :: Sequent -> Text
showSequent s =
    showContext (sequentContext s)
    <> " \\vdash "
    <> showAnnotatedExpr (sequentExpr s) (sequentType s)

showAnnotatedExpr e t = showExpr e <> " : " <> showType t

showContext :: [ContextPart] -> Text
showContext [] = "\\emptyset"
showContext cs = T.intercalate ", " $ map showContextPart cs

showContextPart :: ContextPart -> Text
showContextPart (Gamma Nothing) = "\\Gamma"
showContextPart (Gamma (Just subscriptNum)) =
    showContextPart (Gamma Nothing) <> "_{" <> tshow subscriptNum <> "}"
showContextPart (Binding v t) = showAnnotatedExpr (EVar v) t

showType :: Type -> Text
showType TNum = tt "num"
showType TStr = tt "str"
showType TBool = tt "bool"
showType (TArrow t1 t2) =
    tt "arr" <> "(" <> showType t1 <> " ; " <> showType t2 <> ")"
showType (Tau Nothing) = "\\tau"
showType (Tau (Just subscriptNum)) =
    showType (Tau Nothing) <> "_{" <> tshow subscriptNum <> "}"

showExpr :: Expr -> Text
showExpr (EVar v) = "\\var{" <> v <> "}"
showExpr (ENum x) = showType TNum <> "[" <> tshow x <> "]"
showExpr (EFunc name []) = tt name
showExpr (EFunc name es) =
    tt name <> "(" <> T.intercalate " ; " (map showExpr es) <> ")"
showExpr (ELet e1 v e2) =
    tt "let" <> "(" <> showExpr e1 <> " ; "
    <> showExpr (EVar v) <> " . " <> showExpr e2 <> ")"
