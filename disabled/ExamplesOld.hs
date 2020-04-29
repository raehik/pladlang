{-# LANGUAGE OverloadedStrings #-}

module LaTypInf.ExamplesOld where

import LaTypInf.Types
import LaTypInf.TypeCheck
import qualified LaTypInf.Derivation.AST as DerivAST
import qualified LaTypInf.Derivation.Renderer.Latex as RendererLatex
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as B
import System.IO

writeRenderedDerivation (DerivAST.RenderedDerivationText rd) =
    T.hPutStrLn stdout rd
writeRenderedDerivation (DerivAST.RenderedDerivationBinary rd) =
    B.hPut stdout rd

printRule :: Either Err (Maybe Type, DerivAST.Rule) -> IO ()
printRule (Left err) = print err
printRule (Right (_, rule)) = writeRenderedDerivation $ RendererLatex.renderLatex rule

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

assessEx1 = deriveAndPrint $
    ELet
        (ENum 3)
        "z"
        (ETimes
            (EVar "z")
            (ENum 2))

assessEx2 = deriveAndPrint $
    EPlus
        (ETimes
            (ENum 12)
            (ENum 0))
        (ELen
            (EStr "hi"))

assessEx3Simpler = deriveAndPrint $
    ELet
        (ENum 3)
        "x"
        (ETimes
            (EVar "x")
            (ELen
                (ECat
                    (EStr "hello")
                    (EStr "world"))))

assessAppendixAhh = deriveAndPrint $
    ELet
        (ENum 3)
        "x"
        (ELet
            (ELam TNum "y"
                (ETimes
                    (EVar "x")
                    (EVar "y")))
            "tX"
            (ELet
                (EAp
                    (EVar "tX")
                    (ELen
                        (ECat
                            (EStr "hello")
                            (EStr "world"))))
                "y"
                (EAp
                    (EVar "tX")
                    (EVar "y"))))
