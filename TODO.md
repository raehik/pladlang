## Parsing
  * write a parser for named functions with an arbitrary no. of exprs
    * can use for plus, times, cat, len, equal, if, ap
    * defo won't work in type system, maybe can still do it neatly piecemeal?
      * or maybe it *would* work if I do applicative magic?! maybe :O

## Unsorted
  * consider storing environment in an ordered list - slower, but may want to
    retain order
    * (it's a list in the derivation AST for that reason)
  * perhaps `plus`-like errors need to be shown by `\\ \red{type mismatch}` or
    something, I don't think it actually shows up on the derivation! (due to how
    I handle failures)
  * convert spaces in vars, strings in TeX to that special visible space
    character
