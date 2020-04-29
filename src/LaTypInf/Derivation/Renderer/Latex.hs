-- | Render a type derivation AST as a string of LaTeX macros, using
-- @mathpartir@ for rule typesetting.
--
-- To then render the output with LaTeX, you should need the following in your
-- preamble:
--
-- > \usepackage{mathpartir}
-- > \usepackage{xcolor}
-- > \usepackage{amssymb}
--
-- @amssym@ is only used for its nicer-looking empty set @\\varnothing@. You can
-- remove it by changing that to @\\emptyset@. (I chose the former because it
-- was used by the professor.)
--
-- @mathpartir@ must be used in math mode. I suggest placing derivations in
-- display math mode i.e. @\\[ ... \\]@.

{-# LANGUAGE OverloadedStrings #-}

module LaTypInf.Derivation.Renderer.Latex (
    renderLatex,
    Config,
    configDefault
) where

import LaTypInf.Derivation.AST
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Trans.Reader

--------------------------------------------------------------------------------
-- | Render a rule to LaTeX.
--
-- The derivation is built bottom-up, with branches ending upon sequent-only
-- rules. No tail-call recursion, we hope that the expression isn't absurdly big
-- (mathpartir isn't good at handling large derivations anyway).
renderLatex :: Config -> Rule -> RenderedDerivation
renderLatex cfg rule = RenderedDerivationText $ runReader (showRule rule) cfg

--------------------------------------------------------------------------------
-- | This renderer's configuration.
data Config = Config {
    -- | Whether to use the @amssymb@ set symbol @\\varnothing@, often
    -- recommended over the default one @\\emptyset@.
    configUseAmssymbRounderSetSymbol :: Bool
} deriving (Show)

configDefault = Config {
    configUseAmssymbRounderSetSymbol = True
}

--------------------------------------------------------------------------------
type Renderer = Reader Config

showRule :: Rule -> Renderer Text
showRule (ValidRule r) = do
    name <- showRuleName (validRuleName r)
    judgement <- showSequent (validRuleJudgement r)
    premises <- showPremises (validRulePremises r)
    constructInferRule (Just name) premises judgement
showRule (InvalidRule r) = do
    premises <- showTypeError (invalidRuleError r)
    judgement <- showSequent (invalidRuleJudgement r)
    constructInferRule Nothing premises judgement
showRule (SequentOnly s) = showSequent s

constructInferRule :: Maybe Text -> Text -> Text -> Renderer Text
constructInferRule optName premises judgement = return $
    "\\inferrule*"
    <> case optName of { Nothing -> ""; Just name -> name }
    <> "{" <> premises <> "}"
    <> "{" <> judgement <> "}"

showRuleName :: Text -> Renderer Text
showRuleName name = return $ "[Left=" <> name <> "]"

showPremises :: [Rule] -> Renderer Text
showPremises [] = return " "
showPremises rs = do
    premises <- mapM showRule rs
    return $ T.intercalate " \\\\ " premises

showTypeError :: TypeError -> Renderer Text
showTypeError (TypeErrorUndefinedVariableUsed v) = return $
    "\\textcolor[rgb]{1.0, 0.13, 0.32}{" <> v <> " \\ " <> mathtt "undecl.}"
showTypeError (TypeErrorArgWrongType t) = do
    tRendered <- showType t
    return $  "\\textcolor[rgb]{1.0, 0.13, 0.32}{" <> tRendered <> "}"
showTypeError (TypeErrorAny) = return "type error"

showSequent :: Sequent -> Renderer Text
showSequent s = do
    context <- showContext (sequentContext s)
    annotatedExpr <- showAnnotatedExpr (sequentExpr s) (sequentType s)
    return $ context <> " \\vdash " <> annotatedExpr

showAnnotatedExpr :: Expr -> Type -> Renderer Text
showAnnotatedExpr e t = do
    eRendered <- showExpr e
    tRendered <- showType t
    return $ eRendered <> " : " <> tRendered

showContext :: [ContextPart] -> Renderer Text
showContext [] = do
    useAmssymbRounderSetSymbol <- asks configUseAmssymbRounderSetSymbol
    return $ case useAmssymbRounderSetSymbol of
        True  -> "\\varnothing"
        False -> "\\emptyset"
showContext cs = do
    contextParts <- mapM showContextPart cs
    return $ T.intercalate ", " contextParts

showContextPart :: ContextPart -> Renderer Text
showContextPart (Gamma Nothing) = return "\\Gamma"
showContextPart (Gamma (Just subscriptNum)) = do
    gammaStart <- showContextPart (Gamma Nothing)
    return $ gammaStart <> "_{" <> tshow subscriptNum <> "}"
showContextPart (Binding v t) = showAnnotatedExpr (EVar v) t

showType :: Type -> Renderer Text
showType TNum = return $ mathtt "num"
showType TStr = return $ mathtt "str"
showType TBool = return $ mathtt "bool"
showType (TArrow t1 t2) = do
    t1Rendered <- showType t1
    t2Rendered <- showType t2
    return $ mathtt "arr" <> "(" <> t1Rendered <> " ; " <> t2Rendered <> ")"
showType (Tau Nothing) = return "\\tau"
showType (Tau (Just subscriptNum)) = do
    tauStart <- showType (Tau Nothing)
    return $ tauStart <> "_{" <> tshow subscriptNum <> "}"

showExpr :: Expr -> Renderer Text
showExpr (EVar v) = return $ "\\textcolor[rgb]{0.0, 0.5, 1.0}{" <> v <> "}"
showExpr (ENum x) = do
    t <- showType TNum
    return $ t <> "[" <> tshow x <> "]"
showExpr (EStr x) = do
    t <- showType TStr
    return $ t <> "[" <> x <> "]"
showExpr (EFunc name []) = return $ mathtt name
showExpr (EFunc name es) = do
    exprs <- mapM showExpr es
    return $ mathtt name <> "(" <> T.intercalate " ; " exprs <> ")"
showExpr (ELet e1 v e2) = do
    e1Rendered <- showExpr e1
    vRendered <- showExpr (EVar v)
    e2Rendered <- showExpr e2
    return $ mathtt "let" <> "("
        <> e1Rendered <> " ; "
        <> vRendered <> " . "
        <> e2Rendered <> ")"
showExpr (ELam t v e) = do
    tRendered <- showType t
    vRendered <- showExpr (EVar v)
    eRendered <- showExpr e
    return $ mathtt "lam"
        <> "\\{" <> tRendered <> "\\}"
        <> "(" <> vRendered <> " . "
        <> eRendered <> ")"
showExpr (E Nothing) = return "e"
showExpr (E (Just subscriptNum)) = do
    eStart <- showExpr (E Nothing)
    return $ eStart <> "_{" <> tshow subscriptNum <> "}"

--------------------------------------------------------------------------------
-- Show -> Text.
tshow :: Show a => a -> Text
tshow = T.pack . show

-- Wrap a string in \mathtt.
mathtt :: Text -> Text
mathtt t = "\\mathtt{" <> t <> "}"
