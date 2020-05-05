
import System.IO
import Options.Applicative
import qualified Pladlang.AST as AST
import qualified Pladlang.Parser.LangEF as ParserEF
import qualified Pladlang.Parser.SyntaxSelect as ParserSyntaxSelect
import qualified Pladlang.TypeCheck as TypeCheck
import qualified Pladlang.Derivation.Renderer.Latex as RendererLatex
import qualified Pladlang.Derivation.AST as DerivAST
import qualified Pladlang.ExampleExprs as Examples
import Options
import qualified Pladlang.Parser.Utils as PU
import qualified Text.Megaparsec as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as B
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Void
import qualified Data.List.NonEmpty as NE

type Execution = ReaderT Options IO

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
    opts <- liftIO $ Options.parse
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

-- | Run a function in the context of parsed options.
--withOptions :: Execution a -> Execution a
--withOptions =
--    execParserWithDefaults >>= runReaderT mainNewTwo
