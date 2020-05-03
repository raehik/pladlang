
import System.IO
import Options.Applicative
import qualified Pladlang.AST as AST
import qualified Pladlang.Parser.LangEF as ParserEF
import qualified Pladlang.Parser.SyntaxSelect as ParserSyntaxSelect
import qualified Pladlang.TypeCheck as TypeCheck
import qualified Pladlang.Derivation.Renderer.Latex as RendererLatex
import qualified Pladlang.Derivation.AST as DerivAST
import qualified Pladlang.ExampleExprs as Examples
import qualified Pladlang.Parser.Utils as PU
import qualified Text.Megaparsec as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as B
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Data.Void
import qualified Data.List.NonEmpty as NE

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

data Err
    = ErrParseError (M.ParseErrorBundle Text Void)
    | ErrTypeDerivError TypeCheck.Err
    deriving (Eq, Show)

main :: IO ()
main = do
    ret <- runExceptT main'
    case ret of
        Left err ->
            case err of
                ErrParseError bundle -> putStr . M.errorBundlePretty $ bundle
        Right result ->
            case result of
                ResPladlangExpr expr -> print expr
                ResDerivation deriv -> writeRenderedDerivation deriv

data Result
    = ResPladlangExpr AST.Expr
    | ResDerivation DerivAST.RenderedDerivation
    deriving (Show, Eq)

p = PU.sc *> (NE.head <$> ParserSyntaxSelect.pSyntaxSelectBlocks) <* M.eof

main' :: ExceptT Err IO Result
main' = do
    opts <- liftIO $ execParserWithDefaults parseOpts
    expr <- liftIO $ T.hGetContents stdin
    --ast <- withExceptT ErrParseError $ return $ M.parse ParserEF.parseExpr "stdin" expr
    parseOut <- return $ M.parse p "stdin" expr
    case parseOut of
        Left err -> throwE $ ErrParseError err
        Right ast ->
            case TypeCheck.getTypeDerivation ast of
                Left err -> throwE $ ErrTypeDerivError err
                Right (_, rule) ->
                    case optRenderer opts of
                        RLatex cfg ->
                            let parsedExpr = RendererLatex.renderLatex cfg rule in
                            return $ ResDerivation parsedExpr

parseOpts :: Parser Options
parseOpts = Options <$>
    hsubparser
        (  metavar "RENDERER"
        <> commandGroup "Available renderers:"
        <> latexRendererCmd )

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
