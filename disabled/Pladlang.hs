{-# LANGUAGE OverloadedStrings #-}

module HMInfer.Lang.Pladlang where

import Pladlang.AST
import TypeDeriv.AST
import qualified TypeDeriv.Renderer.Latex as TD
import Data.Text (Text)
import qualified Data.Text as T

render :: Rule Expr Type -> Text
render = TD.render TD.defCfg renderExpr renderType

renderExpr :: TD.Config -> Expr -> Text
renderExpr cfg = walk cfg 0
  where
    assoc' cfg = assoc (walk cfg) prec betweenSpaces

    walk _ _   (EMeta anno)   =
        if anno == T.empty
        then "e"
        else "e_{" <> anno <> "}"
    walk _ _   (EVar v)       = "\\textcolor[rgb]{0.0, 0.5, 1.0}{" <> v <> "}"
    walk _ _   (ENum x)       = tshow x
    walk _ _   (EStr x)       = "\"" <> x <> "\""
    walk _ _    ETrue         = mathtt "true"
    walk _ _    EFalse        = mathtt "false"
    walk cfg _ (ELen e)       = "|" <> walk cfg 0 e <> "|"
    walk cfg _ (EIf e el er)  =
        mathtt "if"
        <> walk cfg 0 e
        <> mathtt "then"
        <> walk cfg 0 el
        <> mathtt "else"
        <> walk cfg 0 er
    walk cfg _ (ELet e1 v e2) =
        mathtt "let"
        <> walk cfg 0 (EVar v)
        <> mathtt "be"
        <> walk cfg 0 e1
        <> mathtt "in"
        <> walk cfg 0 e2
    walk cfg _ (ELam t v e) =
        "\\lambda"
        <> walk cfg 0 (EVar v)
        <> " : " <> renderType cfg t
        <> " . " <> walk cfg 0 e
    walk cfg p e@(EPlus  l r) = assoc' p e l r AssocL "+"
    walk cfg p e@(ETimes l r) = assoc' p e l r AssocL "*"
    walk cfg p e@(ECat   l r) = assoc' p e l r AssocL "++"
    walk cfg p e@(EEqual l r) = assoc' p e l r AssocN "=="
    walk cfg p e@(EAp    l r) = assoc' p e l r AssocL "+"
    walk _   _ _ = "(unsupported expression)"

    prec :: Expr -> Int
    prec (EPlus _ _) = 3
    prec (ETimes _ _) = 4
    prec (ECat _ _) = 2
    prec (EEqual _ _) = 1
    prec (EAp _ _) = 5
    prec _ = 0

renderType :: TD.Config -> Type -> Text
renderType cfg = walk cfg 0
  where
    assoc' cfg = assoc (walk cfg) prec betweenSpaces

    walk _ _ (TMeta anno)   =
        if anno == T.empty
        then "\\tau"
        else "\\tau_{" <> anno <> "}"
    walk _ _    TNum          = mathtt "num"
    walk _ _    TStr          = mathtt "str"
    walk _ _    TBool         = mathtt "bool"
    walk cfg p e@(TArrow l r) = assoc' cfg p e l r AssocR "->"

    prec :: Type -> Int
    prec (TArrow _ _) = 1
    prec _ = 0

betweenSpaces :: Text -> Text
betweenSpaces t
  | t == T.empty = " "
  | otherwise    = T.cons ' ' . flip T.snoc ' ' $ t

tshow :: Show a => a -> Text
tshow = T.pack . show

mathtt :: Text -> Text
mathtt x = "\\mathtt{" <> x <> "}"

data Associativity
    = AssocN
    | AssocL
    | AssocR
    deriving (Eq, Show)

-- Adds associativity to an expression pretty printer.
assoc :: (Int -> a -> Text) -> (a -> Int) -> (Text -> Text) -> Int -> a -> a -> a -> Associativity -> Text -> Text
assoc walk prec opWrapper pp e l r a op
  | pc < pp = paren e
  | otherwise = case a of
        AssocN -> walk2 l <> opWrapper op <> walk2 r
        AssocL -> walk1 l <> opWrapper op <> walk2 r
        AssocR -> walk2 l <> opWrapper op <> walk1 r
  where
    pc = prec e
    paren e' = "(" <> walk pc e' <> ")"
    walk1 = walk pc
    walk2 e'
      | pc == prec e' = paren e'
      | otherwise = walk1 e'
