# SICP Solutions and Study Guide

This is a set of solutions and a [study guide](STUDY.md) for the book ["Structure And Interpretation Of Computer Programs"](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-4.html#%_toc_start)
(SICP), using [the Racket language](https://racket-lang.org/).

To start a REPL with the solution to a particular exercise preloaded
do this:

```
racket -f ch1/1-16-fastexp.scm -i
```

Typically no output will be produced, you will need to type in some
Racket code in the REPL to check that the program works as required by
the given exercise, for example:

```scheme
> (fast-expt 2 3)
8
```

Read the [study guide](STUDY.md) or [more detailed notes on the solutions](NOTES.md).