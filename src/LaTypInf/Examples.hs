-- | Example expressions for testing purposes. Validly typed expressions are
-- prefixed with @valid@, invalid ones with @invalid@.
{-# LANGUAGE OverloadedStrings #-}

module LaTypInf.Examples where

import LaTypInf.Types

--------------------------------------------------------------------------------
validSimplePlus =               EPlus (EPlus (ENum 1) (ENum 2)) (ENum 3)
validSimplePlusLet =            ELet (ENum 1) "x" (EPlus (ENum 1) (EVar "x"))
invalidPlusLetWrongVarLate =    ELet (ENum 1) "x" (EPlus (ENum 1) (EVar "y"))
invalidPlusLetWrongVarEarly =   ELet (ENum 1) "x" (EPlus (EVar "y") (ENum 1))
