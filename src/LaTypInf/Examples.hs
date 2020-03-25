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
exIPlusLetWrongVarLate = deriveAndPrint $ ELet (ENum 1) "x" (EPlus (ENum 1) (EVar "y"))
exIPlusLetWrongVarEarly = deriveAndPrint $ ELet (ENum 1) "x" (EPlus (EVar "y") (ENum 1))
exIClassEFEx4 = deriveAndPrint $
    ELet
        (ELam TNum "x"
            (EAp
                (EVar "f")
                (EPlus
                    (EVar "x")
                    (ENum 1))))
        "f"
        (EAp
            (EVar "f")
            (ENum 0))
exVClassEFEx2 = deriveAndPrint $
    ELet
        (ELam TNum "x"
            (ELam TNum "y"
                (ETimes
                    (EVar "x")
                    (EVar "y"))))
        "f"
        (EAp
            (EAp
                (EVar "f")
                (ENum 4))
            (ENum 4))
