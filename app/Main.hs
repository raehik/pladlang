module Main where

import System.IO
-- import qualified Options.Applicative as OA
import qualified LaTypInf.TypeCheck as TypeCheck
import qualified LaTypInf.Derivation.Renderer.Latex as RendererLatex
import qualified LaTypInf.Derivation.AST as DerivAST
import qualified LaTypInf.Examples as Examples
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as B

render = RendererLatex.renderLatex RendererLatex.configDefault
expr = Examples.validSimplePlus

main :: IO ()
main =
    case TypeCheck.getTypeDerivation expr of
        Left err -> print err
        Right (_, rule) -> writeRenderedDerivation $ render rule

writeRenderedDerivation (DerivAST.RenderedDerivationText rd) =
    T.hPutStrLn stdout rd
writeRenderedDerivation (DerivAST.RenderedDerivationBinary rd) =
    B.hPut stdout rd
