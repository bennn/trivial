#lang scribble/sigplan

@title{Usage}

@; regexp-match
@; format
@; math
@; vectors
@; arity


@section{Database}
@; db

In the years since 2004, language-integrated query has been a popular
 topic.
Languages like Scala, @tt{C#}, and @tt{F*} have fast and type-safe
 interfaces for working with SQL databases.

@; SO WHAT ARE WE DOING HERE

@; Hey, Racket / typed Racket does not fit with the overalll story of
@; dependent-like types

Racket's @racket[db] library provides a direct connection to the database,
 modulo some important safety checks.

Typed Racket programmers may use the @racket[db] library by giving type signatures
 specialized to their needs.
For example, the statement below asserts that @racket[query-row] always returns
 a natural number and a string.
In contrast, @racket[query-maybe-row] can optionally fail, but otherwise returns
 a natural number and a string.

@racketblock[
  (require/typed db
    (#:opaque Connection connection?)
    (sql-connect (->* () (#:user String #:database String) Connection))
    (query-row (-> Connection String Any * (Vector Natural String)))
    (query-maybe-row (-> Connection String Any * (Option (Vector Natural String))))
    ....)
]

To manage additional tables, the programmer can re-import @racket[db] identifiers
 with a fresh name and type signature.

@todo{coherent example}
@racketblock[
  (require/typed (prefix-in w: db)
    (w:query-row (-> Connection String Any * (Vector Natural String))))

  (require/typed (prefix-in s: db)
    (s:query-row (-> Connection String Any * (Vector Natural Natural))))
]

This approach works, but is tedious and error-prone.
Our contribution is to associate a type signature with database connections.
Calls to generic operations like @racket[query-row] are then specialized to
 the exact result type.

@; PERKS
@; - Single point-of-control
@; - lightweight
@; - no changes to underlying library / type system
@; - compile-time validation of query strings
