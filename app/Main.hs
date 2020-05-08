{-# LANGUAGE LambdaCase #-}

import System.IO
import qualified Pladlang.AST as AST
import qualified Pladlang.Parser.SyntaxSelect as ParserSyntaxSelect
import qualified Pladlang.TypeCheck as TypeCheck
import qualified Pladlang.Derivation.Renderer.Latex as RendererLatex
import qualified Pladlang.Derivation.AST as DerivAST
import Options
import qualified Pladlang.Parser.Utils as PU
import qualified Text.Megaparsec as M
import Data.Text (Text)
import qualified Data.Text.IO as T
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
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
handleProgramError = \case
    ErrParseError errBundle -> liftIO . putStr $ M.errorBundlePretty errBundle
    err -> (liftIO . print) err
handleProgramResult :: Result -> ReaderT Options.Options IO ()
handleProgramResult (ResDerivation res) = writeResDerivation res
handleProgramResult res = (liftIO . print) res

writeResDerivation :: DerivAST.RenderedDerivation -> ReaderT Options.Options IO ()
writeResDerivation (DerivAST.RenderedDerivationText res) =
    (liftIO . T.putStrLn) res
writeResDerivation res = (liftIO . print) res

program :: Program Result
program = do
    liftIO (T.hGetContents stdin)
    -- >>= parseAsSyntaxSelect >>= return . ResPladlangExpr
    >>= parseAsSyntaxSelect
    >>= typeDeriveExpr
    >>= renderTypeDerivation

parseAsSyntaxSelect :: Text -> Program AST.Expr
parseAsSyntaxSelect = withExceptT ErrParseError . except . M.parse parser "<stdin>"
  where
    parser = NE.head <$> PU.topParse ParserSyntaxSelect.pSyntaxSelectBlocks

typeDeriveExpr :: AST.Expr -> Program (Maybe AST.Type, DerivAST.Rule)
typeDeriveExpr = withExceptT ErrTypeDerivError . except . TypeCheck.getTypeDerivation

renderTypeDerivation :: (Maybe AST.Type, DerivAST.Rule) -> Program Result
renderTypeDerivation (_, deriv) = do
    renderer <- lift $ asks optRenderer
    case renderer of
        RLatex cfg ->
            let parsedExpr = RendererLatex.renderLatex cfg deriv in
            return $ ResDerivation parsedExpr
