# SICP Solutions and Study Guide

This is a set of solutions and a study guide for the book ["Structure And Interpretation Of Computer Programs"](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-4.html#%_toc_start)
(SICP), using [the Racket language](https://racket-lang.org/).

To start a REPL with the solution to a particular exercise preloaded
do this:

```
racket -f ch1/1-17-multiply-double.scm -i
```

No output will be produced, you will need to type in some Racket code
in the REPL to check that the program works as required by the given
exercise.

## Notes & caveats

* [Racket](https://racket-lang.org/) is a programming language, and
  also a programming environment capable of running many different
  programming languages that are not Racket. The main Racket language
  is based on Scheme, which is what was originally used in the book,
  but now it is a language on its own. Fortunately, since SICP
  purposefully uses a very spartan subset of Scheme, the code in the
  book is trivial to port to the Racket language. Racket has the
  advantage of being very actively maintained and developed, making it
  easy to work with it in a modern computing environment, e.g. it
  provides a unit testing framework and good GUI capabilities. You
  could use the Scheme language in the Racket environment but I do not
  see much advantage in doing this.

* There are cases in the book where the authors ask you to code a
  solution making use of some code which will only be presented later
  in the book. I tend to provide complete working programs for each
  exercise, which makes it easy to run and test the solution, but has
  the downside of resulting in lengthy solution files even when the
  change done in a particular given exercise was very small.

## Resources for studying SICP

* [Complete book contents online](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-4.html#%_toc_start)

* [Racket](https://racket-lang.org/)

* [1986 video lectures for the first edition of the book](https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video-lectures/), by Abelson & Sussman, the authors of the book

* [CS61A class page](https://cs61a.org/), a class at Berkeley that is based SICP

* [CS61A video lectures](https://archive.org/details/ucberkeley-webcast-PL3E89002AA9B9879E?sort=titleSorter), with Brian Harvey, well-renowed

* [Eli Bendersky's solutions and notes on the book](http://eli.thegreenplace.net/tag/sicp)

* [Prof. Sussman's "Adventures in Advanced Symbolic Programming" class notes](https://groups.csail.mit.edu/mac/users/gjs/6.945/), building and extending on the material in SICP. Make sure to check out the readings and pdf's for programming assignments, plenty of interesting stuff there.

* [Prof. Sussman's reading list](http://aurellem.org/thoughts/html/sussman-reading-list.html) with many positions more or less related to SICP.

* [We Really Don't Know How to Compute](https://www.youtube.com/watch?v=O3tVctB_VSU), [The Role of Programming](https://www.youtube.com/watch?v=arMH5GjBwUQ), and [The Legacy of Computer Science](https://www.youtube.com/watch?v=6J1vRrozgBg) - Prof. Sussman's video lectures given on various occasions, touching upon some of the themes from SICP.