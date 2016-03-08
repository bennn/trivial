#lang scribble/sigplan

@title{Implementation}

@; - syntax parse
@; - 
@; - identifier macros
@; - let-bindings, make=rename-transformer
@; - define-bindings, free-id-table
@; - local expand
@; - phasing (we have + at all levels)

@; We don't re-implement format or regexp,
@; but we do implement + and some vector operations, to go faster






@section{Correctness}

Our implementation of @racket[format] in @todo{figure-ref} exhibits a few desirable properties.
@itemlist[
  @item{}
]

Generally speaking these properties are the ``right'' way to judge if a transformation is correct.



Open question: can we design a restricted macro language where every transformation
 is statically checked to ensure termination and correctness.

We have argued that our macros help typed programming, but they did so only
 by going outside the law of the type checker.

