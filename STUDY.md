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
Scheme](https://docs.racket-lang.org/r5rs/running.html), but this just
seems to complicate everything - REPL is worse, it is not clear how to
unit test your code etc., so I recommend sticking to both
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

## Choosing an editor/IDE

Racket includes an IDE called
[DrRacket](http://docs.racket-lang.org/quick/index.html), and has very
good
[documentation](https://docs.racket-lang.org/guide/other-editors.html)
on Racket extensions for text editors.

## Background knowledge from mathematics and electronics

The book uses mathematics and electronics for a lot of the example
programs, without giving any background knowledge on those topics,
which might scare some people off. My most important advice on this:
**do not panic**. Only a very cursory understanding of those
disciplines is required for most of the exercises, you do not have to
do a lot of additional study, although some background reading will
certainly increase appreciation. I recommend lcamtuf's [Concise
electronics for geeks](http://lcamtuf.coredump.cx/electronics/) for
electronics, and 3Blue1Brown [Essence of calculus video
series](https://www.youtube.com/watch?v=WUvTyaaNkzM&list=PLZHQObOWTQDMsr9K-rj53DwVRMYO3t5Yr)
for mathematics, as the quickest way to get the necessary
background.

On your first pass through the book, I would recommend trying to
maintain a steady tempo, even if this means skipping some exercises or
solving them without fully understanding all the details. Ideas
typically form a graph in which many nodes are mutally dependent, and
so you should not expect to learn everything at once when going
through a textbook which is by necessity linear - by trying too hard
to do this you will only loose motivation. A second and third pass
will make everything make much more sense without anywhere as much
effort, especially having spent some months or years in between
working on or learning anything related to the book topics. This is
the [spiral
approach](https://www.av8n.com/physics/spiral-approach.htm) to
learning.

For me a big part of the value from studying SICP was the further
interests it sparked and the broad intellectual perspective it gave me
on all of those topics. It is worth noticing that some of the book
themes apply to mathematics and electronics as well as they apply to
computer programming. If you actually enjoy learning about those
topics, I recommend eventually also reading the books [What is
Mathematics?](https://www.amazon.com/Mathematics-Elementary-Approach-Ideas-Methods/dp/0195105192)
and [The Art of
Electronics](https://www.amazon.com/Art-Electronics-Paul-Horowitz/dp/0521809266/ref=dp_ob_title_bk)
which are the closest to being SICP-equivalents for their respective
fields.

## Generally useful resources

* [Online version of the SICP book](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-4.html#%_toc_start)

* [Beautiful unofficial online version of the book](http://sarabander.github.io/sicp/)

* [Differences between Racket and Scheme](https://stackoverflow.com/questions/3345397/how-is-racket-different-from-scheme), explained on StackOverflow

* [6.001 OpenCourseware class page](https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/), based on the MIT class the book was originally written for

* [1986 video lectures for the first edition of the book](https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video-lectures/), by Abelson & Sussman, the authors of the book

* [CS61A class page](https://cs61a.org/), a class at Berkeley that is based SICP

* [CS61A video lectures](https://archive.org/details/ucberkeley-webcast-PL3E89002AA9B9879E?sort=titleSorter), with Brian Harvey, well-renowed

* [Eli Bendersky's solutions and notes on the book](http://eli.thegreenplace.net/tag/sicp)

* [Adventures in Advanced Symbolic Programming](https://groups.csail.mit.edu/mac/users/gjs/6.945/), Prof. Sussman's class extending upon the material in SICP. Make sure to check out the readings and pdf's for programming assignments, plenty of interesting stuff there.

* [Prof. Sussman's reading list](http://aurellem.org/thoughts/html/sussman-reading-list.html) with many positions more or less related to SICP.

* [We Really Don't Know How to Compute](https://www.youtube.com/watch?v=O3tVctB_VSU), [The Role of Programming](https://www.youtube.com/watch?v=arMH5GjBwUQ), and [The Legacy of Computer Science](https://www.youtube.com/watch?v=6J1vRrozgBg), lectures by Prof. Sussman's given on various occasions, touching upon some of the themes from SICP.

* [Essence of calculus video series](https://www.youtube.com/watch?v=WUvTyaaNkzM&list=PLZHQObOWTQDMsr9K-rj53DwVRMYO3t5Yr), mathematics background reading

* [Concise electronics for geeks](http://lcamtuf.coredump.cx/electronics/), electronics background reading