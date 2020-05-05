## Parsing
  * add option to be strict on identifier naming (no keywords)
  * fix tokenizing: require spaces between them? it's amusing that it works
    without in the first place (incorrect lexeme-ing?)
    * lexeme usage seems fine but I don't really get lots of this anyway sooo
    * well actually it's breaking tokens that of prefixes of other tokens...
  * add EAp parsing -- it's weird and I can't figure it out
    * binary op but the key is, there is no op, it's empty
    * needs to be left-associative

## Unsorted
  * consider storing environment in an ordered list - slower, but may want to
    retain order
    * (it's a list in the derivation AST for that reason)
  * perhaps `plus`-like errors need to be shown by `\\ \red{type mismatch}` or
    something, I don't think it actually shows up on the derivation! (due to how
    I handle failures)
  * convert spaces in vars, strings in TeX to that special visible space
    character
