
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
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Either.Combinators
import Data.Void
import qualified Data.List.NonEmpty as NE

type Program = ExceptT Err (ReaderT Options.Options IO)

data Result
    = ResPladlangExpr AST.Expr
    | ResDerivation DerivAST.RenderedDerivation
    deriving (Eq, Show)

data Err
    = ErrParseError (M.ParseErrorBundle Text Void)
    | ErrTypeDerivError TypeCheck.Err
    deriving (Eq, Show)

main :: IO ()
main = Options.parse >>= runReaderT (runExceptT program >>= either handleProgramError handleProgramResult)

handleProgramError :: Err -> ReaderT Options.Options IO ()
handleProgramError _ = liftIO $ putStrLn "error boyo"
handleProgramResult :: Result -> ReaderT Options.Options IO ()
handleProgramResult _ = liftIO $ putStrLn "success! of some sort"

writeRenderedDerivation (DerivAST.RenderedDerivationText rd) =
    T.hPutStrLn stdout rd
writeRenderedDerivation (DerivAST.RenderedDerivationBinary rd) =
    B.hPut stdout rd

p = PU.sc *> (NE.head <$> ParserSyntaxSelect.pSyntaxSelectBlocks) <* M.eof

program :: Program Result
program = do
    liftIO (T.hGetContents stdin)
    >>= parseAsSyntaxSelect
    >>= typeDeriveExpr
    >>= renderTypeDerivation

parseAsSyntaxSelect :: Text -> Program AST.Expr
parseAsSyntaxSelect = withExceptT ErrParseError . except . M.parse p "<stdin>"
typeDeriveExpr :: AST.Expr -> Program (Maybe AST.Type, DerivAST.Rule)
typeDeriveExpr = withExceptT ErrTypeDerivError . except . TypeCheck.getTypeDerivation
renderTypeDerivation :: (Maybe AST.Type, DerivAST.Rule) -> Program Result
renderTypeDerivation (_, deriv) = do
    renderer <- lift $ asks optRenderer
    case renderer of
        RLatex cfg ->
            let parsedExpr = RendererLatex.renderLatex cfg deriv in
            return $ ResDerivation parsedExpr
