{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module LaTypInf.TypesOld where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Reader
import qualified Data.List as List

tshow :: Show a => a -> Text
tshow = T.pack . show

type Binding = (VVar, Type)

type VVar = Text
type VNum = Int
type VStr = Text
data VBool = VTrue | VFalse deriving (Show)

data Type
    = TNum
    | TStr
    | TBool
    | TArrow Type Type
instance Show Type where
    show TNum = "\\mathtt{num}"
    show TStr = "\\mathtt{str}"
    show TBool = "\\mathtt{bool}"
    show (TArrow t1 t2) = "\\mathtt{arr} (" ++ show t1 ++ " ; " ++ show t2 ++ ")"

data Expr a where
    EVar    :: VVar -> Expr VVar
    ENum    :: VNum -> Expr VNum
    EStr    :: VStr -> Expr VStr
    EBool   :: VBool -> Expr VBool
    EPlus   :: Expr VNum -> Expr VNum -> Expr VNum
instance (Show a) => Show (Expr a) where
    show (EVar x) = T.unpack x
    show (ENum x) = "\\mathtt{num}[" ++ show x ++ "]"
    show (EStr x) = "\\mathtt{str}[" ++ T.unpack x ++ "]"
    show (EBool VTrue) = "\\mathtt{true}"
    show (EBool VFalse) = "\\mathtt{false}"

showBindings :: [Binding] -> Text
showBindings [] = "\\emptyset"
showBindings bs = foldl concatBinding T.empty bs
    where
        concatBinding str (v, t)
            | str == T.empty = bindingStr
            | otherwise = str <> ", " <> bindingStr
            where
                bindingStr = v <> " : " <> tshow t

showTypeDerivation :: Expr a -> Text
showTypeDerivation (EVar x) = x
showTypeDerivation (ENum x) = "\\mathtt{num}[" <> tshow x <> "]"
showTypeDerivation (EStr x) = "\\mathtt{str}[" <> x <> "]"
showTypeDerivation (EBool x) = tshow x
showTypeDerivation (EPlus e1 e2)
    = "\\inferrule*[Left=ef-ty-plus]"
        <> "{\\emptyset \\vdash e_1 : \\mathtt{num} \\\\"
        <> " \\emptyset \\vdash e_2 : \\mathtt{num} }"
        <> "{\\emptyset \\vdash \\mathtt{plus} ("
        <> showTypeDerivation e1
        <> " ; "
        <> showTypeDerivation e2
        <> ") : \\mathtt{num}}"

printTypeDerivation e = putStrLn $ T.unpack $ runReader (deriveType e) []

deriveType :: Expr a -> Reader [Binding] Text
deriveType (EStr x) = do
    bindings <- ask
    let rule =
            "\\inferrule*[Left=\\textbf{EF}-ty-str]"
            <> "{ }"
            <> "{" <> showBindings bindings <> " \\vdash " <> tshow (EStr x) <> " : " <> tshow TStr <> "}"
    return rule
deriveType (ENum x) = do
    bindings <- ask
    let rule =
            "\\inferrule*[Left=\\textbf{EF}-ty-num]"
            <> "{ }"
            <> "{" <> showBindings bindings <> " \\vdash " <> tshow (ENum x) <> " : " <> tshow TNum <> "}"
    return rule
deriveType (EPlus e1 e2) = do
    bindings <- ask
    let rule =
            "\\inferrule*[Left=\\textbf{EF}-ty-num]"
            <> "{ }"
            <> "{" <> showBindings bindings <> " \\vdash " <> tshow (EPlus e1 e2) <> " : " <> tshow TNum <> "}"
    return rule
