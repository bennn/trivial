#lang scribble/sigplan

@title[#:tag "sec:conclusion"]{Closing the Books}

@; TODO yuck approach to ... to
This pearl described an approach to using a macro system to enhance
 the analysis of an existing type system.
Whereas types support reasoning independent of data representations,
 we hope to have shown that reflecting a little value information
 into the compile-time environment can pay off for common programming tasks.

Indeed, a tempting subtitle for this pearl would be
 @emph{compile-time constant propagation}, because that is precisely what we do.
Each application in @Secref{sec:usage} is a way that constant data
 can be turned into type constraints.

A key thesis behind this work is that the analysis can be implemented
 in a variety of languages using their existing type and syntax extension
 systems. Indeed:

@parag{Typed Clojure} has

@parag{Haskell}
@parag{OCaml}
@parag{Rust}
@parag{Scala}

With out last words, we look to the future.
Although we gave criteria for correct predicates and translations
 in @Secref{sec:solution}, we only claimed that the functions
 shown in @Secref{sec:implementation} are correct.
Short of proving operational equivalence, could we concisely or automatically
 show that our translations are correct?
Slightly more pressingly, can we relive the burden of propagating syntax properties?
