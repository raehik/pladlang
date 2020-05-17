## Parsing
  * parse top expression context
  * clean up metavar syntax
    * maybe allow empty `$` as empty var, empty type

## Derivation
  * rewrite AST to be simpler
    * actually, no, rewrite it to still be as flexible as it is, but extend
      types to include more generic constructors and use those internally where
      possible
    * our derivation renderers are gonna have to have a number of different
      flavours now: latex-ef, latex-efast (unless I can somehow do another
      intermediate step?)

## Unsorted
  * consider storing environment in an ordered list - slower, but may want to
    retain order
    * (it's a list in the derivation AST for that reason)
  * perhaps `plus`-like errors need to be shown by `\\ \red{type mismatch}` or
    something, I don't think it actually shows up on the derivation! (due to how
    I handle failures)
  * convert spaces in vars, strings in TeX to that special visible space
    character
