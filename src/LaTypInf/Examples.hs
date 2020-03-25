{-# LANGUAGE OverloadedStrings #-}

module LaTypInf.Examples where

import LaTypInf.Types
import LaTypInf.TypeCheck
import qualified LaTypInf.Derivation.AST as DerivAST
import qualified LaTypInf.Derivation.Converter.Latex as DerivConvLatex
import Data.Text (Text)
import qualified Data.Text as T

printRule :: Either Err (Maybe Type, DerivAST.Rule) -> IO ()
printRule (Left err) = tprint (tshow err)
printRule (Right (_, rule)) = tprint $ DerivConvLatex.showRule rule

tprint :: Text -> IO ()
tprint = putStrLn . T.unpack

deriveAndPrint = printRule . getTypeDerivation

exVPlusNums = deriveAndPrint $ EPlus (EPlus (ENum 1) (ENum 2)) (ENum 3)
exVPlusLet = deriveAndPrint $ ELet (ENum 1) "x" (EPlus (ENum 1) (EVar "x"))
exIPlusLetWrongVar = deriveAndPrint $ ELet (ENum 1) "x" (EPlus (ENum 1) (EVar "y"))
