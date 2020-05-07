# Design notes
## CLI commands
**Overall program description:** Parse an expression and render its type
derivation or reduction.

We also allow parsing a type derivation or reduction directly, allowing us to
render rules easily.

  * parse expression -> create type derivation -> render type derivation
    * select parser (no specific options)
    * select renderer, renderer-specific options
  * parse type derivation -> render type derivation
    * (only one parser -- this one needs writing lol, will be fun)
    * select renderer, renderer-specific options
  * parse expression -> create reduction -> render reduction
    * select parser (no specific options)
    * select renderer, renderer-specific options
  * parse reduction -> render reduction
    * (only one parser -- this one needs writing lol, will be fun)
    * select renderer, renderer-specific options

I could *almost* write a lots of what you'd want into the expression parsers
themselves... but sadly, the language doesn't allow you to indicate context. But
if I added *that*, I'm thinking I could do it. I'd need lots of extra syntax for
indicating metavariables.

## CLI
  * renderer = command (because they all have their own flags)

## Parsers
### Languages (expressions)
All these parsers are of type `Parser AST.Expr` (take `Text`, output a Pladlang
expression).

TODO, none of this is true.

The general syntax is as follows:

  * `<ws>` is short for `<whitespace>`.

```bnf
<top-expr> ::= <ws> <expr> <ws>
<expr>  ::=
      '(' <ws> <expr> <ws> ')' <ws>
    | "true" <ws>
    | "false" <ws>
    | <integer> <ws>
    | <var>
<integer> ::= <digit> <integer> | <digit>
<digit> ::= digitChar
<var> ::= alphanumChar
<type>  ::= "num" | "str" | "bool"

<whitespace> ::= whitespaceChar <whitespace> | ''
```

  * `parseLangEFConcrete`
  * `parseLangEFAbstract`

### Parser-select expression file
We want to be able to place multiple expressions in a single file, to simplify
processing. For this, we want a syntax which allows us to declare the
expression's language, rather than selecting based on CLI options or file
extension. Then it's a short hop to parsing multiple expressions, selecting the
relevant parser for each one on-the-fly.
