#lang scribble/sigplan
@require["common.rkt"]

@title[#:tag "sec:conclusion"]{Closing the Books}

@; TODO yuck approach to ... to
This pearl described an approach to using a macro system to enhance
 the analysis of an existing type system.
Whereas types support reasoning independent of data representations,
 we hope to have shown that reflecting a little value information
 into the compile-time environment can pay off for common programming tasks.
@; Moreover, can do without programmer annotations

Indeed, a tempting subtitle for this pearl would be
 @emph{compile-time constant propagation}, because that is precisely what we do.
Each application in @Secref{sec:usage} is a way that constant data
 can be turned into type constraints.

A key thesis behind this work is that the analysis can be implemented
 in a variety of languages using their existing type and syntax extension
 systems. Indeed:

@parag{Typed Clojure} has a flexible macro system derived from Common Lisp.
Built-in functions @tt{macroexpand} and @tt{with-meta} correspond to Racket's
 @racket[local-expand] and @racket[syntax-property]; with help from a library
 implementing identifier macros,@note{@url["https://github.com/clojure/tools.macro"]}
 we were able to implement a basic prototype in untyped Clojure.

@todo{does TC have macros? can I use macros from untyped code if I type them?}


@parag{Haskell} Template Haskell


@parag{OCaml} ppx


@parag{Rust} has a stable, pattern-based macro system which unfortunately
 seems too weak to reimplement our analysis.
As of Rust 1.7,@note{@url["https://doc.rust-lang.org/reference.html"]}
 the basic macros can only specify input/output pairs.
There is no way to do complex branching within a condition,
 such as throwing an exception if a format string is given too many arguments.
Just parsing the format string would be challenging.

However Rust also has an interface for writing arbitrarily powerful compiler
 plugins.
It would be interesting to reproduce our results as a plugin.


@parag{Scala} users have a few macro systems to choose from.
@; We investigated Scala Macros and TODO.
@; https://doc.rust-lang.org/book/compiler-plugins.html


With out last words, we look to the future.
Although we gave criteria for correct predicates and translations
 in @Secref{sec:solution}, we only claimed that the functions
 shown in @Secref{sec:implementation} are correct.
Short of proving operational equivalence, could we concisely or automatically
 show that our translations are correct?
Slightly more pressingly, can we relive the burden of propagating syntax properties?
