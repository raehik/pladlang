
import System.IO
import Options.Applicative
import qualified Pladlang.TypeCheck as TypeCheck
import qualified Pladlang.Derivation.Renderer.Latex as RendererLatex
import qualified Pladlang.Derivation.AST as DerivAST
import qualified Pladlang.ExampleExprs as Examples
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as B

render = RendererLatex.renderLatex RendererLatex.defaultConfig
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

progDesc' :: String
progDesc' = "Type derive expressions in a simple language."

progVer :: String
progVer = "0.1"

main :: IO ()
main = do
    opts <- execParserWithDefaults parseOpts
    print opts

parseOpts :: Parser Options
parseOpts = Options <$>
    hsubparser
        (metavar "RENDERER"
        <> commandGroup "Available renderers:"
        <> latexRendererCmd
        )

latexRendererCmd :: Mod CommandFields Renderer
latexRendererCmd =
    command
        "latex"
        (info (RLatex <$> RendererLatex.options) (progDesc "Render derivation to LaTeX."))

-- Run a parser with a few set preferences and --version, --help options.
execParserWithDefaults :: Parser a -> IO a
execParserWithDefaults parser =
    customExecParser (prefs $ showHelpOnError <> noBacktrack) $ decorateParser parser
  where
    decorateParser :: Parser a -> ParserInfo a
    decorateParser parser =
        info
            (helper <*> versionOpt <*> parser)
            (fullDesc <> progDesc progDesc')
    versionOpt :: Parser (a -> a)
    versionOpt = infoOption progVer (long "version" <> help "Show version")
