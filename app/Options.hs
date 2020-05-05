-- | Pladlang executable CLI option parser.

module Options where

import Options.Applicative
import qualified Pladlang.Derivation.Renderer.Latex as RendererLatex

data Options = Options {
    optRenderer :: Renderer
} deriving (Eq, Show)

data Renderer
    = RLatex RendererLatex.Config
    deriving (Eq, Show)

desc :: String
desc = "Type derive expressions in a simple language."

-- | TODO have this connected to elsewhere
ver :: String
ver = "0.3"

parse :: IO Options
parse = execParserWithDefaults pOpts

pOpts :: Parser Options
pOpts = Options <$>
    hsubparser
        (  metavar "RENDERER"
        <> commandGroup "Available renderers:"
        <> latexRendererCmd )

latexRendererCmd :: Mod CommandFields Renderer
latexRendererCmd =
    command
        "latex"
        (info (RLatex <$> RendererLatex.options) (progDesc "Render derivation to LaTeX."))

-- | Run a parser with a few set preferences and --version, --help options.
execParserWithDefaults :: Parser a -> IO a
execParserWithDefaults parser =
    customExecParser (prefs $ showHelpOnError <> noBacktrack) $ decorateParser parser
  where
    decorateParser :: Parser a -> ParserInfo a
    decorateParser parser =
        info
            (helper <*> versionOpt <*> parser)
            (fullDesc <> progDesc desc)
    versionOpt :: Parser (a -> a)
    versionOpt = infoOption ver (long "version" <> help "Show version")
