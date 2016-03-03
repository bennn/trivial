#lang scribble/sigplan

@require["common.rkt"]

@title{How its made}

We describe our technique in terms of four functions on syntax objects.
Our implementation is in Typed Racket, but we believe
      Clojure,
      Haskell,
      Javascript,
      OCaml,
      Rust,
  and Scala
 could reproduce our results.@note{We list two non-essential difficulties in
  @todo{secref}.}
@; The hope in listing these languages up-front is to spark readers' imagination
@; before we give our own solution in Section 3


