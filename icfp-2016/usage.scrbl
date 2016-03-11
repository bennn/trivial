#lang scribble/sigplan
@require["common.rkt"]

@title[#:tag "sec:usage"]{Real-World Metaprogramming}

We have defined useful letter-of-values transformations for a variety of
 common programming tasks ranging from type-safe string formatting to
 constant-folding of arithmetic.
These transformations are implemented in Typed Racket@~cite[TypedRacket],
 which inherits Racket's powerful macro system@~cite[plt-tr1].

Our exposition does not assume any knowledge of Racket or Typed Racket, but
 a key design choice of the Typed Racket language model bears mentioning:
 evaluation of a Typed Racket program happens across three distinct stages,
 shown in @Figure-ref{fig:staging}.
First the program is read and macro-expanded; as expanding a macro introduces
 new code, the result is recursively read and expanded until no macros remain.
Next, the @emph{fully-expanded} Typed Racket program is type checked.
If checking succeeds, types are erased and the program is handed to the Racket
 compiler.
For us, this means that we can implement @exact|{$\trans\ $}|
 functions as macros referencing @exact|{$\interp\ $}| functions and rely
 on the macro expander to invoke our transformations before the type checker runs.

@figure["fig:staging"
  @list{Typed Racket language model}
  @exact|{\input{fig-staging}}|
]

Though we describe each of the following transformations using in-line constant
 values, our implementation applies @exact|{$\interp\ $}| functions to every
 definition and let-binding in the program and then associates compile-time
 data with the bound identifier.
When a defined value flows into a function like @racket[printf] without being
 mutated along the way, we retrieve this cached information.
The macro system features used to implement this behavior are described in
 @Secref{sec:implementation}.

@; regexp-match
@; format
@; math
@; vectors
@; arity

@section{Regexp}
Regular expression patterns are another common value whose structure is not
 expressed by conventional type systems.
Consider the function @racket[regexp-match] from Typed Racket:

@interaction[
  (regexp-match #rx"types" "types@ccs.neu.edu")

  (regexp-match #rx"lambda" "types@ccs.neu.edu")

  (regexp-match #rx"(.*)@(.*)" "types@ccs.neu.edu")
]
@; TODO note that nested groups cannot fail.

When called with a pattern @racket[p] and string @racket[s] to match with,
 @racket[(regexp-match p s)] returns a list of all substrings in @racket[s]
 matching groups in the pattern @racket[p].
The pattern itself counts for one group and parentheses within @racket[p]
 delimit nested groups.
Hence the third example returns a list of three elements.
A match can also fail, in which case the value @racket[#f] is returned.

Although Typed Racket's @racket[regexp-match] has the type@note{Assuming that a
    successful match implies that all nested groups successfully match.}
 @racket[(Regexp String -> (U #f (Listof String)))], the number of strings
 in the result list is determined by the number of groups in the input regular
 expression.
Like the arity of a function or the size of a tuple, the number of groups
 in a pattern is often clear to the programmer.
We could imagine using
 an indexed family of @racket[regexp-match] functions for patterns of
 two, three, or more groups.
Ideally though, the programming language should understand
 these unconventional or domain-specific flavors of polymorphism.


@section{Database}
@; db
@; TODO Ocaml, Haskell story

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

Incidentally, @racket[regexp-match:] is useful for parsing queries.
We also implemented @racket[query-row] to return a sized vector.


While light years away from LINQ, this technique provides at least basic
 safety guarantees for an existing library and demonstrates the generality of
 our technique.


@section{}
@; identifier macros
@; rename transformers
