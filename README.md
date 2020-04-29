# UKC CS BSc CO663 2020: LaTeX formal type derivation builder
[ctan-mathpartir]: https://ctan.org/pkg/mathpartir
[ukc-co663]: https://www.kent.ac.uk/courses/modules/module/CO663

LaTypInf takes an equation written in a simple language inspired by the
lambda calculus, and builds a formal type derivation for it in LaTeX macros via
the [mathpartir][ctan-mathpartir] package.

This was written to cheat on part of an assessment for the [CO663 Programming
Languages: Applications and Design][ukc-co663] module in UKC's 2020 CS BSc
course, and cleaned up post submission.

## Usage
LaTypInf is built with [Stack](https://www.haskellstack.org), so with it
installed:

    stack ghci

Then run wild. Examples are stored in `LaTypInf.Examples` and prepended with
`ex` or `assess`.

To compile the LaTeX output, you'll need LaTeX with a bunch of packages
including [mathpartir][ctan-mathpartir].

## License
Provided under the MIT license. Please see [LICENSE](LICENSE) for the full
license text.
