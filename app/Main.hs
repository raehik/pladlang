module Main where

import System.IO
import Options.Applicative
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

{-
main :: IO ()
main =
    case TypeCheck.getTypeDerivation expr of
        Left err -> print err
        Right (_, rule) -> writeRenderedDerivation $ render rule
-}

writeRenderedDerivation (DerivAST.RenderedDerivationText rd) =
    T.hPutStrLn stdout rd
writeRenderedDerivation (DerivAST.RenderedDerivationBinary rd) =
    B.hPut stdout rd

data Options = Options {
    optRenderer :: Renderer
} deriving (Show)

data Renderer
    = RLatex RendererLatex.Config
    deriving (Show)

main :: IO ()
main = do
    opts <- execParser optsParser
    print opts
  where
    optsParser :: ParserInfo Options
    optsParser =
        info
            (helper <*> versionOption <*> programOptions)
            (fullDesc <> progDesc "Type derive expressions in a simple language.")
    versionOption :: Parser (a -> a)
    versionOption = infoOption "0.0" (long "version" <> help "Show version")
    programOptions :: Parser Options
    programOptions = Options <$> hsubparser (latexRenderer <> metavar "RENDERER")
    latexRenderer :: Mod CommandFields Renderer
    latexRenderer =
        command
            "latex"
            (info (RLatex <$> RendererLatex.options) (progDesc "Render derivation to LaTeX"))
