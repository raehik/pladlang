# Design notes
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
