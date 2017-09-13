# SICP Study Guide

## Choosing a language and interpreter

I recommend using [Racket](https://racket-lang.org/).

Racket is a programming language, and at the same time a programming
environment capable of running many different programming languages
that are not Racket. The main Racket language is based on Scheme,
which is what was originally used in the book, but now it is a
language on its own with [some incompatible changes and
extensions](https://stackoverflow.com/questions/3345397/how-is-racket-different-from-scheme).
Fortunately, since SICP very purposefully uses a spartan subset of
Scheme, the code from the book is trivial to port to the Racket
language.

Racket has the advantage of being very actively maintained and
developed, making it easier to install and use by people used to
modern programming environments, in comparison to "standard" Scheme
implementations. Racket is [capable of running programs in "standard"
Scheme](https://docs.racket-lang.org/r5rs/running.html), but this
seems to just complicate everything - REPL is worse, it is not clear
how to unit test your code etc., so I recommend sticking to both
Racket-the-environment and Racket-the-language.

You could also use [Clojure](https://clojure.org/), or a modern Common
Lisp implementation like [SBCL](http://www.sbcl.org/), but the code
would significantly diverge from what is in the book. [MIT
Scheme](https://www.gnu.org/software/mit-scheme/) was used in the
6.001 class for which SICP was written, but it does not see the kind
of active development Racket does.

Note that the point of the book is to implement many sophisticated
programming concepts like objects, lazy evaluation, logic programming
etc. using the simplest means like `let`, `lambda`, `cons`, `car`,
`cdr`, so I purposefully do not use any advanced Racket language
features in my solutions. I do make use of the libraries, for example
for unit tests.

## Additional resources

* [Complete book contents online](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-4.html#%_toc_start)

* [Differences between Racket and Scheme](https://stackoverflow.com/questions/3345397/how-is-racket-different-from-scheme), explained on StackOverflow

* [1986 video lectures for the first edition of the book](https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video-lectures/), by Abelson & Sussman, the authors of the book

* [CS61A class page](https://cs61a.org/), a class at Berkeley that is based SICP

* [CS61A video lectures](https://archive.org/details/ucberkeley-webcast-PL3E89002AA9B9879E?sort=titleSorter), with Brian Harvey, well-renowed

* [Eli Bendersky's solutions and notes on the book](http://eli.thegreenplace.net/tag/sicp)

* [Prof. Sussman's "Adventures in Advanced Symbolic Programming" class notes](https://groups.csail.mit.edu/mac/users/gjs/6.945/), building and extending on the material in SICP. Make sure to check out the readings and pdf's for programming assignments, plenty of interesting stuff there.

* [Prof. Sussman's reading list](http://aurellem.org/thoughts/html/sussman-reading-list.html) with many positions more or less related to SICP.

* [We Really Don't Know How to Compute](https://www.youtube.com/watch?v=O3tVctB_VSU), [The Role of Programming](https://www.youtube.com/watch?v=arMH5GjBwUQ), and [The Legacy of Computer Science](https://www.youtube.com/watch?v=6J1vRrozgBg) - Prof. Sussman's lectures given on various occasions, touching upon some of the themes from SICP.