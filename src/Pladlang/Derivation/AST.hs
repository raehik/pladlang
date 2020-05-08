{- | The intermediate type derivation syntax.

Type checking an expression produces a derivation AST, which can be rendered out
into a useful format e.g. LaTeX.

This AST also provides "generic" parts using metavars (gamma, tau) instead of
actual values, for displaying type rules. These parts won't be used when doing
the type derivation for an expression -- if you want to, you must write the AST
directly.
-}

module Pladlang.Derivation.AST where

import Data.Text (Text)
import qualified Data.ByteString as B

data TypeError
    = TypeErrorUndefinedVariableUsed Text
    | TypeErrorArgWrongType Type
    | TypeErrorAny
    deriving (Show)

data Rule
    = ValidRule ValidRule'
    | InvalidRule InvalidRule'

    -- | A rule with no premises (axiom-like). Ends a derivation branch.
    | SequentOnly Sequent

    deriving (Show)

-- | Valid rules are formed of a judgement, a list of premises, and the name of
-- the rule applied (to display at the side).
data ValidRule' = ValidRule' {
    validRuleName :: Text,
    validRulePremises :: [Rule],
    validRuleJudgement :: Sequent
} deriving (Show)

-- | Invalid rules do not have names associated, and provide a 'TypeError' in
-- place of premises.
data InvalidRule' = InvalidRule' {
    invalidRuleError :: TypeError,
    invalidRuleJudgement :: Sequent
} deriving (Show)

-- | A statement being derived (the lower bit in a rule).
data Sequent = Sequent {
    sequentContext :: [ContextPart],
    sequentExpr :: Expr,
    sequentType :: Type
} deriving (Show)

data ContextPart
    -- | A regular context binding.
    = Binding Text Type

    -- | Context metavar. The optional integer appends a subscript number.
    | Gamma (Maybe Integer)

    deriving (Show)

data Expr
    = EVar Text
    | ENum Integer
    | EStr Text
    | EFunc Text [Expr]
    | ELet Expr Text Expr
    | ELam Type Text Expr
    -- | Expression metavar.
    | EMeta Text
    deriving (Show)

data Type
    = TNum
    | TStr
    | TBool
    | TArrow Type Type
    -- | Type metavar.
    | TMeta Text
    deriving (Show)

-- | Renderers must wrap their output in this type to indicate whether it's
-- textual or binary data.
data RenderedDerivation
    = RenderedDerivationText Text
    | RenderedDerivationBinary B.ByteString
    deriving (Show, Eq)
