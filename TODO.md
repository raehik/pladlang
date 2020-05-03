## Parsing
  * add option to be strict on identifier naming (no keywords)

## Unsorted
  * consider storing environment in an ordered list - slower, but may want to
    retain order
    * (it's a list in the derivation AST for that reason)
  * perhaps `plus`-like errors need to be shown by `\\ \red{type mismatch}` or
    something, I don't think it actually shows up on the derivation! (due to how
    I handle failures)
  * convert spaces in vars, strings in TeX to that special visible space
    character
