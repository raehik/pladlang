# Pladlang: Tools for a simple exemplar programming language
[ctan-mathpartir]: https://ctan.org/pkg/mathpartir
[ukc-co663]: https://www.kent.ac.uk/courses/modules/module/CO663

This is a set of tools for Pladlang, a generalisation of some simple lambda
calculus-like languages introduced in the 2020 UKC module [CO663 Programming
Languages: Application and Design][ukc-co663] (-> PLAD). It started off as a
formal type derivation builder to let me cheat on part of an assessment, and
grew much later as I learned more Haskell.

Main usage: Takes an equation written in a simple language inspired by the
lambda calculus and build a formal type derivation for it in LaTeX macros via
the [mathpartir][ctan-mathpartir] package.

## Usage
Pladlang is built with [Stack](https://www.haskellstack.org), so with it
installed:

    stack ghci

Then run wild. Examples are stored in `Pladlang.Examples` and prepended with
`ex` or `assess`. (TODO)

To compile the LaTeX output, you'll need LaTeX with a bunch of packages
including [mathpartir][ctan-mathpartir]. (TODO)

## Acknowledgements
Summaries of the languages implemented in Pladlang were originally provided as
course material for the 2020 CO663 module at UKC.

Language EF is adapted from: *Harper, Robert. Practical foundations for
programming languages. Cambridge University Press, 2016.*

## License
Provided under the MIT license. Please see [LICENSE](LICENSE) for the full
license text.
