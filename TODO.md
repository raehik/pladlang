## Derivation
  * oh, re-add errors - I think I have to do the Rule' thing again
    * *looks* like I don't need SequentOnly (I can simply use no premises), but
      I certainly do need to differentiate between valid and invalid rules
    * potentially I could change premises to `Either (TypeError e t) [Rule e t]`
      * I've thought this through REISEINI (no IME) & it seems to work. And the
        types line up to very nicely - we have ultimate freedom via an DT which
        has the expr+type type! e.g. embedding expressions in type errors, and
        having the user choose how they're rendered

## Derivation rendering
  * for rendering to CS: how to figure out correct bracketing without just
    overdoing it?? :(

## Parsing
  * parse top expression context
  * clean up metavar syntax
    * maybe allow empty `$` as empty var, empty type

## Unsorted
  * consider storing environment in an ordered list - slower, but may want to
    retain order
    * (it's a list in the derivation AST for that reason)
  * perhaps `plus`-like errors need to be shown by `\\ \red{type mismatch}` or
    something, I don't think it actually shows up on the derivation! (due to how
    I handle failures)
  * convert spaces in vars, strings in TeX to that special visible space
    character
