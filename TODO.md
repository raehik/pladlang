  * consider storing environment in an ordered list - slower, but may want to
    retain order
    * (it's a list in the derivation AST for that reason)

## Reorganise monad transformer stack
  * `typeCheck :: Parser (Either TypeError Type)`
  * reorganise monad transformer stack
    * need state for derivation rules
    * stay in `ExceptT TypeError`, but we make the decision whether to rethrow
      or continue when we get a type error (so we can end immediately, or
      continue type checking to build up the rest of the derivation)
