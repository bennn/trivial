#lang scribble/sigplan
@(require "common.rkt" racket/string racket/sandbox)

@; TODO 'need to' etc
@; TODO clever examples
@; TODO definition pane for examples
@; +------------
@; | f \in \interp
@; |
@; | > (f x)
@; | y
@; +------------
@; TODO -- maybe a better title is "what can we learn from values"?
@; TODO remove all refs to implementation?


@title[#:tag "sec:usage"]{Real-World Metaprogramming}
@;@(define base-eval
@;  (make-evaluator 'racket/base 
@;    (sandbox-output 'string)
@;    (sandbox-error-output 'string)
@;    (error-display-handler (lambda (x y) (displayln "wepa")))))

We have defined useful letter-of-values transformations for a variety of
 common programming tasks ranging from type-safe string formatting to
 constant-folding of arithmetic.
These transformations are implemented in Typed Racket@~cite[TypedRacket],
 which inherits Racket's powerful macro system@~cite[plt-tr1].
For us, this means that Typed Racket comes equipped with powerful tools
 for analyzing and transforming the syntax of programs before any type checking
 occurs.

@;Our exposition does not assume any knowledge of Racket or Typed Racket, but
@; a key design choice of the Typed Racket language model bears mentioning:
@; evaluation of a Typed Racket program happens across three distinct stages,
@; shown in @Figure-ref{fig:staging}.
@;First the program is read and macro-expanded; as expanding a macro introduces
@; new code, the result is recursively read and expanded until no macros remain.
@;@; TODO stronger distinction between read/expand and typecheck
@;Next, the @emph{fully-expanded} Typed Racket program is type checked.
@;If checking succeeds, types are erased and the program is handed to the Racket
@; compiler.
@;
@;For us, this means that we can implement @exact|{$\trans$}|
@; functions as macros and rely on the macro expander to invoke our
@; transformations before the type checker runs.
@;The transformations can use @exact|{$\interp$}| functions as
@; needed to complete their analysis.
@;
@;@figure["fig:staging"
@;  @list{Typed Racket language model}
@;  @exact|{\input{fig-staging}}|
@;]


@parag{Conventions}
Interpretation and transformation functions are defined over syntactic expressions
 and values.
To make clear the difference between Typed Racket terms and syntactic representations
 of terms, we quote the latter and typeset it in green.
Using this notation, @racket[(λ (x) x)] is a term implementing the identity
 function and @racket['(λ (x) x)] is a syntactic object that will evaluate
 to the identity function.
Values are typeset in green because their syntax and term representations are identical.

In practice, syntax objects carry lexical information.
Such information is extremely important, but to simplify our presentation we
 pretend it does not exist.
Think of this convention as removing the oyster shell to get a clear view of the pearl.

Using infix @tt{:} for type annotations, for instance @racket[(x : Integer)].
These are normally written as @racket[(ann x Integer)].

sql is short for postgresql


@; =============================================================================
@section{String Formatting}

@; TODO add note about the ridiculous survey figure? Something like 4/5 doctors
Format strings are the world's second most-loved domain-specific language (DSL).
@; @~cite[wmpk-algol-1968]
At first glance, a format string is just a string; in particular, any string
 used as the first argument in a call to @racket[printf].
But between the quotation marks, format strings may contain @emph{format directives}
 that tell @emph{where} and @emph{how} values can be spliced into the format string.
@; TODO scheme or common lisp?
Racket follows the Lisp tradition@~cite[s-lisp-1990] of using a tilde character (@tt{~})
 to prefix format directives.
For example, @racket[~s] converts any value to a string and @racket[~b] converts a
 number to binary form.

@interaction[
    (printf "binary(~s) = ~b" 7 7)
]

If the format directives do not match the arguments to @racket[printf], most
 languages fail at run-time@~cite[a-icfp-1999].
This is a simple kind of value error that should be caught statically.

@; TODO print errors nicer
@interaction[
    (printf "binary(~s) = ~b" "7" "7")
]

Detecting inconsistencies between a format string and its arguments is easy
 provided we have a function @racket[fmt->types] @exact|{$\in \interp$}| for
 reading types from a format string.
In Typed Racket this function is rather simple because the most common
 directives accept @code{Any} type of value.
Such are the joys of uniform syntax---printing is free.

@racketblock[
> (fmt->types "binary(~s) = ~b")
'[Any Integer]
> (fmt->types '(λ (x) x))
#false
]

Now to use @racket[fmt->types] in a function @racket[t-printf] @exact|{$\in \trans$}|.
Given a call to @racket[printf], we validate the number of arguments and
 add type annotations derived using @racket[fmt->types].
For all other syntax patterns, @racket[t-printf] is the identity transformation.

@racketblock[
> (t-printf '(printf "~a"))
⊥
> (t-printf '(printf "~b" "2"))
'(printf "~b" ("2" : Integer))
> (t-printf printf)
'printf
]

The first example is rejected immediately as a syntax error.
The second is temporarily accepted, but will cause a static type error.
Put another way, the format string @racket{~b} specializes the type of
 @racket[printf] from @racket[(String Any * -> Void)] to @racket[(String Integer -> Void)].
The third is slightly more interesting; it demonstrates that higher-order
 uses of @racket[printf] default to the standard behavior.


@; =============================================================================
@section{Regular Expressions}
Moving now from the second most-loved DSL to the first, regular expressions
 are often used to capture sub-patterns within a string.

@racketblock[
> (regexp-match #rx"(.*)@(.*)" "toni@merchant.net")
'("toni@merchant.net" "toni" "merchant.net")
]

The first argument to @racket[regexp-match] is a regular expression pattern.
Inside the pattern, the parentheses delimit sub-pattern @emph{groups}, the dots match
 any single character, and the Kleene star matches zero-or-more repetitions
 of the pattern-or-character preceding it.
The second argument is a string to match against the pattern.
If the match succeeds, the result is a list containing the entire matched string
 and substrings corresponding to each group captured by a sub-pattern.
If the match fails, Racket's @racket[regexp-match] returns @racket[#false].

@racketblock[
> (regexp-match #rx"-(2*)-" "111-222-3333")
'("-222-" "222")
> (regexp-match #rx"¥(.*)" "$2,000")
#false
]

Certain groups can also fail to capture even when the overall match succeeds.
This can happen, for example, when a group is followed by a Kleene star.

@racketblock[
> (regexp-match #rx"(a)*(b)" "b")
'("b" #f "b")
]

Therefore, a simple catch-all type for @racket[regexp-match] is
 @racket[(Regexp String -> (U #f (Listof (U #f String))))].
Using the general type, however, is cumbersome for simple patterns
 where a match implies that all groups will successfully capture.

@;@(define tr-eval (make-base-eval '(require typed/racket/base racket/list)))
@racketblock[
> (define (get-domain [full-name : String]) : String
    (cond
     [(regexp-match #rx"(.*)@(.*)" full-name)
      => third]
     [else "Match Failed"]))
]
@;Error: expected String, got (U #false String)
@; `third` could not be applied to arguments
@; Arguments: (Listof (U #false String))
@; Expected Result: String

Analysing the parentheses contained in a regular expression pattern can often
 determine the number of groups statically.
We implement this as a function @racket[rx->groups] @exact|{$\in \interp$}|

@racketblock[
> (rx->groups #rx"(a)(b)(c)")
3
> (rx->groups #rx"((a)b)")
2
> (rx->groups #rx"(a)*(b)")
#false
]

@; TODO can we not talk about casts?
The corresponding transformation
 @racket[t-regexp] @exact|{$\in \trans$}|
 inserts casts to refine the type of results
 produced by @racket[regexp-match].
It also flags malformed groups in uncompiled regular expressions.

@racketblock[
> (t-regexp '(regexp-match #rx"(a)b" str))
'(cast (regexp-match #rx"(a)b" str)
       (U #f (List String String)))
> (t-regexp '(regexp-match "(" str))
⊥
]


@; =============================================================================
@section{Procedure Arity}

Anonymous functions are another value form whose representation contains
 useful data.
By tokenizing symbolic λ-expressions, we can parse their domain
 syntactically in a function @racket[fun->domain] @exact|{$\in \interp$}|

@racketblock[
> (fun->arity '(λ (x y z) (x (z y) y)))
'[Any Any Any]
> (fun->arity '(λ ([x : Real] [y : Real]) x))
'[Real Real]
]

When domain information is available at call sites to a @racket[curry] function,
 we can produce code that will pass typechecking.
One method is to swap @racket[curry] with an indexed version,
 as demonstrated by @racket[t-swap] @exact|{$\in \trans$}|

@racketblock[
> (t-swap '(curry (λ (x y) x)))
'(curry_1 (λ (x y) x))
]

Another option is to implement @racket[curry] with unsafe tools from
 the language runtime and assign it the trivial type @racket[(⊥ -> ⊤)]
A transformation would replace this type with a subtype where possible
 and let the type checker reject other calls.
Our implementation employs the first strategy, but generates a new @racket[curry_i]
 at each call site by folding over the inferred function domain.
@; Mention this later, or show code?


@; =============================================================================
@section{Constant Folding}

The identity interpretation is useful for lifting constant values to
 the compile-time environment.
@racket[id] @exact|{$\in \interp$}|

@racketblock[
> (id 2)
2
> (id "~s")
"~s"
]

When composed with a filter, we can recognize classes of constants.
That is, if @racket[int?] is the identity function on integer values
 and the constant function returning @racket[#false] otherwise, we have
 (@racket[int?]@exact{$\,\circ\,$}@racket[id])@exact|{$\,\in \interp$}|

@racketblock[
> (int? (id 2))
2
> (int? (id "~s"))
#false
]

Using this technique we can detect division by the constant zero and
 implement constant-folding transformation of arithmetic operations.
@racket[t-arith] @exact{$\in \interp$}

@racketblock[
> (t-arith '(/ 1 0))
⊥
> (t-arith '(* 2 3 7 z))
'(* 42 z)
]

These transformations are most effective when applied bottom-up, from the
 leaves of a syntax tree to the root.

@racketblock[
> (t-arith '(/ 1 (- 5 5)))
⊥
]

@Secref{sec:implementation} describes how we accomplish this recursion scheme
 with the Racket macro system.
@; The implementation of @racket[t-arith] expects flat/local data.


@; =============================================================================
@section[#:tag "sec:vector"]{Sized Data Structures}

Vector bounds errors are always frustrating to debug, especially since
 their cause is rarely deep.
In many cases, vectors are declared with a constant size and
 accessed at constant positions, plus or minus an offset.@note{Loops do not
  fit this pattern, but many languages already provide for/each constructs
  to streamline common looping patterns.}
But unless the compiler or runtime system tracks vector bounds, the programmer
 must unfold definitions and manually find the source of the problem.

A second, more compelling reason to track vector bounds is to remove the
 run-time checks inserted by memory-safe languages.
If we can statically prove that a reference is in-bounds, we should be able
 to optimize it.

Racket provides a few ways to create vectors.
Length information is explicit in some and derivable for
 others, provided we interpret sub-expressions recursively.
@racket[vector->length] @exact{$\in \interp$}

@racketblock[
> (vector->length '#(0 1 2 3))
3
> (vector->length '(make-vector 100 #true))
100
> (vector->length '(vector-append #(left) #(right)))
7
> (vector->length '(vector-drop #(A B C) 2))
1
]

Once we have sizes associated with compile-time vectors,
 we can define optimized translations of built-in functions.
For the following, assume that @racket[unsafe-ref] is an unsafe version of @racket[vector-ref].
@racket[t-vec] @exact{$\in \trans$}

@racketblock[
> (t-vec '(vector-ref #(Y E S) 2))
'(unsafe-ref #(Y E S) 2)
> (t-vec '(vector-ref #(N O) 2))
⊥
> (t-vec '(vector-length (make-vector 100 #true)))
100
]

We can define vectorized arithmetic using similar translations.
Each operation match the length of its arguments at compile-time and expand
 to an efficient implementation.


@; =============================================================================
@section{Database Queries}
@; db
@; TODO Ocaml, Haskell story

Racket's @racket[db] library provides a string-based API for executing SQL
 queries.
Connecting to a database requires only a username.
Queries are strings, but may optionally reference query parameters---natural numbers
 prefixed by a dollar sign (@tt{$}).
Arguments substituted for query parameters are guarded against SQL injection.

@racketblock[
> (define C (sql-connect #:user "shylock"
              #:database "accounts"))
> (query-row C
    "SELECT amt_owed FROM loans WHERE name = '$1'"
    "antonio")
3000
]

This is a far cry from language-integrated query@~cite[mbb-sigmod-2006], but the interface
 is relatively safe and very simple to use.

Typed Racket programmers may use the @racket[db] library by assigning specific
 type signatures to functions like @racket[query-row].
This is somewhat tedious, as the distinct operations for querying one value,
 one row, or a lazy sequence of rows each need a type.
A proper type signature might express the database library as a functor over the
 database schema, but Typed Racket does not have functors or even existential types.
Even if it did, the queries-as-strings interface makes it impossible for a standard
 type checker to infer type constraints on query parameters.

The situation worsens if the programmer uses multiple database connections.
One can either alias one query function to multiple identifiers (each with a specific type)
 or weaken type signatures and manually type-cast query results.

Our technique provides a solution.
We associate database connections with a compile-time value representing the
 database schema.
Operations like @racket[query-row] then extract the schema from a connection
 and interpret type constraints from the query string.
These constraints are passed to the type system in two forms: as annotations
 on expressions used as query parameters and as casts on the result of a query.
Note that the casts are not guaranteed to succeed---in particular, the schema
 may differ from the actual types in the database.

In total, the solution requires three interpretation functions and at least two
 transformations.
First, we implement a predicate to recognize schema values.
A schema is a list of table schemas; a table schema is a tagged list of
 column names and column types.

(@racket[schema?]@exact{$\,\circ\,$}@racket[id]) @exact{$\in \interp$}
@racketblock[
> (schema? '([loans [(id . Natural)
                     (name . String)
                     (amt_owed . Integer)]]))
'([loans ...])
]

Composed with the identity interpretation, this predicate lifts schema values
 from the program text to the compile-time environment.

@; TODO this must be clearer
@; TODO note that this isn't a drop-in replacement
Next, we need to associate database connections with database schemas.
We do this by transforming calls to @racket[sql-connect]; connections made
 without an explicit schema argument are rejected.

(@racket[sc]) @exact{$\in \trans$}
@racketblock[
> (sc '(sql-connect #:user "shylock"
         #:database "accounts"))
⊥
> (sc '(sql-connect ([loans ...])
         #:user "shylock"
         #:database "accounts"))
'(sql-connect #:user "shylock"
   #:database "accounts")
]

To be consistent with our other examples, we should now describe an
 interpretation from connection objects to schemas.
That is, we should be able to extract the schema for the @racket{accounts} database
 from the syntactic representation of @racket{shylock}'s connection object.
Our implementation, however, defines @racket[connection->schema] @exact{$\in \interp$} as
 @racket[(λ (x) #false)].
Instead, we assume that all connection objects are named and rely on a general
 technique for associating compile-time data with identifiers.
See @Secref{sec:def-overview} for an overview and @Secref{sec:def-implementation}
 for details.

Our final interpretation function tokenizes query strings and returns
 column and table information.
In particular, we parse @tt{SELECT} statements and extract
@itemlist[
  @item{the names of selected columns,}
  @item{the table name, and}
  @item{an association from query parameters to column names.}
]

@racketblock[
> "SELECT amt_owed FROM loans WHERE name = '$1'"
'[(amt_owed) loans ($1 . name)]
> "SELECT * FROM loans"
'[* loans ()]
]

The schema, connection, and query constraints now come together in transformations
 such as @racket[t] @exact{$\in \trans$}.
There is still a non-trivial amount of work to be done resolving wildcards and
 validating table and row names before the type-annotated result is produced,
 but all the necessary information is available.

@racketblock[
> (t '(query-row C
        "SELECT amt_owed FROM loans WHERE name = '$1'"
        "antonio"))
'(cast (query-row C
        "SELECT amt_owed FROM loans WHERE name = '$1'"
        ("antonio" : String))
       (Vector Integer))
> (t '(query-row C
        "SELECT * FROM loans WHERE name = '$1'"
        "antonio"))
'(cast (query-row C
        "SELECT amt_owed FROM loans WHERE name = '$1'"
        ("antonio" : String))
       (Vector Natural String Integer))
> (t '(query-row C "SELECT * FROM itunes"))
⊥
]


@; =============================================================================
@section[#:tag "sec:def-overview"]{Definitions and Let-bindings}

Though we described each of the above transformations using in-line constant
 values, our implementation cooperates with definitions and let bindings.
By means of an example, the difference @racket[(- n m)] in the following program
 is compiled to the value 0.

@racketblock[
> (define m (* 6 7))
> (let ([n m])
    (- m n))
]

Conceptually, we accomplish this by applying known functions
 @exact{${\tt f} \in \interp$} at definition sites and keeping a compile-time
 mapping from identifiers to interpreted data.
Thanks to Racket's meta-programming tools, implementing this table in a way
 that cooperates with aliases and lexical scope is simple.

@; TODO set!, for defines and lets
