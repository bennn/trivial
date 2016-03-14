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
 systems.
Indeed:
@; TODO shorten this

@parag{Typed Clojure} has a flexible macro system derived from Common Lisp.
Built-in functions @tt{macroexpand} and @tt{with-meta} correspond to Racket's
 @racket[local-expand] and @racket[syntax-property]; with help from a library
 implementing identifier macros,@note{@url["https://github.com/clojure/tools.macro"]}
 we were able to implement a basic prototype untyped Clojure.


@parag{Template Haskell} could reproduce all the examples in @Secref{sec:usage}
 as they are written, with constants in-line at each call site.
We are less sure how to associate and retrieve data regarding bound variables
 at compile-time, idiomatically.

@parag{OCaml} ppx


@parag{Rust} has a stable, pattern-based macro system and a powerful API
 for writing compiler plugins.
The macro system appears too weak to reimplement our analyses,
 but it would be interesting to explore the plugin API and try reimplementing
 ideas like syntax classes and rename transformers.


@parag{Scala} users have at least two macro systems to choose from.
Both Scala Macros@~cite[b-scala-2013] and Lightweight Modular Staging@~cite[ro-gpce-2010]
 


With out last words, we look to the future.
Although we gave criteria for correct predicates and translations
 in @Secref{sec:solution}, we only claimed that the functions
 shown in @Secref{sec:implementation} are correct.
Short of proving operational equivalence, could we concisely or automatically
 show that our translations are correct?
Slightly more pressingly, can we relive the burden of propagating syntax properties?
