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



@section{Database}

Note: Typed Racket users can already specialize functions from the @tt{db} library
 by giving them exact types in an import statement:

@racketblock[
  (require/typed db
    (#:opaque Connection connection?)
    (postgresql-connect (->* () (#:user String #:database String) Connection))
    (query-row (-> Connection String Any * (Vector Natural String)))
    (query-maybe-row (-> Connection String Any * (Option (Vector Natural String))))
    ....)
]

The above statement asserts that @racket[query-row] only returns rows containing
 a natural number followed by a string.
To work with multiple tables in the same module, a programmer can re-import
 @racket[db] functions using a different name and type.

@todo{cohernet table names}
@racketblock[
  (require/typed (prefix-in w: db)
    (w:query-row (-> Connection String Any * (Vector Natural String))))

  (require/typed (prefix-in s: db)
    (s:query-row (-> Connection String Any * (Vector Natural Natural))))
]

@todo{Other languages? also follow this approach}

The benefits of our approach are:
@itemlist[
  @item{
    Single point of control for database types, instead of spreading types
    across identifiers in the @racket[require/typed] statement.
  }
  @item{
    Compile-time validation of query strings.
  }
]



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

