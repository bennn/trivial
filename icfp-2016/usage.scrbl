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
@; TODO remove all refs to implementation?

@title[#:tag "sec:usage"]{What we can learn from Values}

We have defined useful elaborators for a variety of
 common programming tasks ranging from type-safe string formatting to
 constant-folding of arithmetic.
These elaborators are implemented in Typed Racket@~cite[TypedRacket], a macro-extensible
 typed language that compiles into Racket@~cite[plt-tr1].
An important component of Typed Racket's design is that all macros in a program
 are fully expanded before type-checking begins.
This protocol lets us implement our elaborators as macros that expand into
 typed code.

@parag{Conventions}
@itemlist[
 @item{
Interpretation and elaboration functions are defined over symbolic expressions
 and values; specifically, over @emph{syntax objects}.
To make clear the difference between Typed Racket terms and representations
 of terms, we quote the latter and typeset it in green.
Using this notation, @racket[(λ (x) x)] is a term implementing the identity
 function and @racket['(λ (x) x)] is a representation that will evaluate
 to the identity function.
Values are typeset in green because their syntax and term representations are identical.
} @item{
In practice, syntax objects carry lexical information.
Such information is extremely important, especially for implementing @racket[define]
 and @racket[let] forms that respect @exact{$\alpha$}-equivalence, but
 to simplify our presentation we omit it.
} @item{
We use an infix @tt{:} to write explicit type annotations and casts,
 for instance @racket[(x : Integer)].
These normally have two different syntaxes, respectively
 @racket[(ann x Integer)] and @racket[(cast x Integer)].
} @item{
  TODO phantom item for space
  TODO phantom item for space
  TODO phantom item for space
  TODO phantom item for space
  TODO phantom item for space
} @item{
In @Secref{sec:sql}, @tt{sql} is short for @tt{postgresql}, i.e.
 the code we present in that section is only implemented for the @tt{postgresql}
 interface.
} @item{
Until @Secref{sec:def-overview}, we use @racket[define] and @racket[let] forms sparingly.
In fact, this practice aligns with our implementation---once the interpretations
 and elaborations are defined, extending them to handle arbitrary definitions
 and renamings is purely mechanical.
}
]

@; =============================================================================
@section{Format Strings}

@; TODO add note about the ridiculous survey figure? Something like 4/5 doctors
@; TODO note regexp is the first?
Format strings are the world's second most-loved domain-specific language (DSL).
@; @~cite[wmpk-algol-1968]
All strings are valid format strings; additionally, a format string may contain
 @emph{format directives} describing @emph{where} and @emph{how} values can be
 spliced into the format string.
@; TODO scheme or common lisp?
Racket follows the Lisp tradition@~cite[s-lisp-1990] of using a tilde character (@tt{~})
 to prefix format directives.
For example, @racket[~s] converts any value to a string and @racket[~b] converts a
 number to binary form.

@exact|{
\begin{SCodeFlow}\begin{RktBlk}\begin{SingleColumn}\Scribtexttt{{\Stttextmore} }\RktPn{(}\RktSym{printf}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{"binary($\sim$s) = $\sim$b"}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{7}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{7}\RktPn{)}

\RktOut{binary(7) = 111}

\begin{SingleColumn}\end{SingleColumn}\end{SingleColumn}\end{RktBlk}\end{SCodeFlow}
}|

If the format directives do not match the arguments to @racket[printf], most
 languages fail at run-time.
This is a simple kind of value error that could be caught statically.

@exact|{
\begin{SCodeFlow}\begin{RktBlk}\begin{SingleColumn}\Scribtexttt{{\Stttextmore} }\RktPn{(}\RktSym{printf}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{"binary($\sim$s) = $\sim$b"}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{"7"}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{"7"}\RktPn{)}

\RktErr{printf: format string requires argument of type $<$exact{-}number$>$}

\begin{SingleColumn}\end{SingleColumn}\end{SingleColumn}\end{RktBlk}\end{SCodeFlow}
}|

Detecting inconsistencies between a format string and its arguments is straightforward
 if we define an interpretation @racket[fmt->types] @exact|{$\in \interp$}| for
 reading types from a format string value.
In Typed Racket this function is rather simple because the most common
 directives accept @code{Any} type of value---in a language with uniform syntax,
 printing comes for free.

@exact|{
\hfill\fbox{\RktMeta{fmt->types} $\in \interp$}

\begin{SCodeFlow}\begin{RktBlk}\begin{SingleColumn}\RktSym{{\Stttextmore}}\mbox{\hphantom{\Scribtexttt{x}}}\RktPn{(}\RktSym{fmt{-}{\Stttextmore}types}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{"binary($\sim$s) = $\sim$b"}\RktPn{)}

\RktVal{{\textquotesingle}}\RktVal{[}\RktVal{Any}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{Integer}\RktVal{]}

\RktSym{{\Stttextmore}}\mbox{\hphantom{\Scribtexttt{x}}}\RktPn{(}\RktSym{fmt{-}{\Stttextmore}types}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{{\textquotesingle}}\RktVal{(}\RktVal{$\lambda$}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{(}\RktVal{x}\RktVal{)}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{x}\RktVal{)}\RktPn{)}

\RktVal{\#false}\end{SingleColumn}\end{RktBlk}\end{SCodeFlow}
}|

Now to use @racket[fmt->types] in an elaboration.
Given a call to @racket[printf], we check the number of arguments and
 add type annotations using the inferred types.
For all other syntax patterns, @racket[t-printf] is the identity elaboration.

@exact|{
\hfill\fbox{$\elabf \in \interp$}

\begin{SCodeFlow}\begin{RktBlk}\begin{SingleColumn}\RktSym{{\Stttextmore}}\mbox{\hphantom{\Scribtexttt{x}}}\RktPn{(}\RktSym{t{-}printf}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{{\textquotesingle}}\RktVal{(}\RktVal{printf}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{"$\sim$a"}\RktVal{)}\RktPn{)}

\RktSym{$\perp$}

\RktSym{{\Stttextmore}}\mbox{\hphantom{\Scribtexttt{x}}}\RktPn{(}\RktSym{t{-}printf}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{{\textquotesingle}}\RktVal{(}\RktVal{printf}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{"$\sim$b"}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{"2"}\RktVal{)}\RktPn{)}

\RktVal{{\textquotesingle}}\RktVal{(}\RktVal{printf}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{"$\sim$b"}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{(}\RktVal{"2"}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{{\hbox{\texttt{:}}}}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{Integer}\RktVal{)}\RktVal{)}

\RktSym{{\Stttextmore}}\mbox{\hphantom{\Scribtexttt{x}}}\RktPn{(}\RktSym{t{-}printf}\mbox{\hphantom{\Scribtexttt{x}}}\RktSym{printf}\RktPn{)}

\RktVal{{\textquotesingle}}\RktVal{printf}\end{SingleColumn}\end{RktBlk}\end{SCodeFlow}
}|

The first example is rejected immediately as a syntax error.
The second is a valid elaboration, but will lead to a static type error.
Put another way, the format string @racket{~b} specializes the type of
 @racket[printf] from @racket[(String Any * -> Void)] to @racket[(String Integer -> Void)].
The third example demonstrates that higher-order
 uses of @racket[printf] default to the standard, unspecialized behavior.


@; =============================================================================
@section{Regular Expressions}
Regular expressions are often used to capture sub-patterns within a string.

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
If the match fails, @racket[regexp-match] returns @racket[#false].

@racketblock[
> (regexp-match #rx"-(2*)-" "111-222-3333")
'("-222-" "222")
> (regexp-match #rx"¥(.*)" "$2,000")
#false
]

Certain groups can also fail to capture even when the overall match succeeds.
This can happen when a group is followed by a Kleene star.

@racketblock[
> (regexp-match #rx"(a)*(b)" "b")
'("b" #f "b")
]

Therefore, a catch-all type for @racket[regexp-match] is fairly large:
 @racket[(Regexp String -> (U #f (Listof (U #f String))))].
Using this general type is cumbersome for simple patterns
 where a match implies that all groups will successfully capture.

@exact|{
\begin{SCodeFlow}\begin{RktBlk}\begin{SingleColumn}\RktSym{{\Stttextmore}}\mbox{\hphantom{\Scribtexttt{x}}}\RktPn{(}\RktSym{define}\mbox{\hphantom{\Scribtexttt{x}}}\RktPn{(}\RktSym{get{-}domain}\mbox{\hphantom{\Scribtexttt{x}}}\RktPn{[}\RktSym{full{-}name}\mbox{\hphantom{\Scribtexttt{x}}}\RktSym{{\hbox{\texttt{:}}}}\mbox{\hphantom{\Scribtexttt{x}}}\RktSym{String}\RktPn{]}\RktPn{)}\mbox{\hphantom{\Scribtexttt{x}}}\RktSym{{\hbox{\texttt{:}}}}\mbox{\hphantom{\Scribtexttt{x}}}\RktSym{String}

\mbox{\hphantom{\Scribtexttt{xxxx}}}\RktPn{(}\RktSym{cond}

\mbox{\hphantom{\Scribtexttt{xxxxx}}}\RktPn{[}\RktPn{(}\RktSym{regexp{-}match}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{\#rx"({\hbox{\texttt{.}}}*)@({\hbox{\texttt{.}}}*)"}\mbox{\hphantom{\Scribtexttt{x}}}\RktSym{full{-}name}\RktPn{)}

\mbox{\hphantom{\Scribtexttt{xxxxxx}}}\RktSym{={\Stttextmore}}\mbox{\hphantom{\Scribtexttt{x}}}\RktSym{third}\RktPn{]}

\mbox{\hphantom{\Scribtexttt{xxxxx}}}\RktPn{[}\RktSym{else}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{"Match Failed"}\RktPn{]}\RktPn{)}\RktPn{)}

\RktErr{Error: expected $<$String$>$, got $<$(U \#false String)$>$}

\end{SingleColumn}\end{RktBlk}\end{SCodeFlow}
}|


We alleviate the need for casts and guards in simple patterns
 with a parentheses-counting
 interpretation that parses regular expressions
 and returns the number of groups.
If there is any doubt whether a group will capture, we default to the general
 @racket[regexp-match] type.

@todo{fbox} @racket[rx->groups] @exact|{$\in \interp$}|

@racketblock[
> (rx->groups #rx"(a)(b)(c)")
3
> (rx->groups #rx"((a)b)")
2
> (rx->groups #rx"(a)*(b)")
#false
]

The corresponding elaboration
 inserts casts to subtype the result of calls to @racket[regexp-match].
It also raises syntax errors when an uncompiled regular expression contains
 unmatched parentheses.

@todo{fbox}

@racketblock[
> (t-regexp '(regexp-match #rx"(a)b" str))
'(cast (regexp-match #rx"(a)b" str)
       (U #f (List String String)))
> (t-regexp '(regexp-match "(" str))
⊥
]


@; =============================================================================
@section{Anonymous Functions}

By tokenizing symbolic λ-expressions, we can interpret their domain
 statically. @todo{fbox} @racket[fun->domain] @exact|{$\in \interp$}|

@racketblock[
> (fun->domain '(λ (x y z) (x (z y) y)))
'[Any Any Any]
> (fun->domain '(λ ([x : Real] [y : Real]) x))
'[Real Real]
]

When domain information is available at calls to a @racket[curry] function,
 we elaborate to a type-correct, indexed version of @racket[curry].
Conceptually, we give @racket[curry] the unusable type @racket[(⊥ -> ⊤)] and
 elaboration produces a subtype @racket[curry_i].

@todo{fbox} @exact|{$\in \elab$}|

@racketblock[
> ('(curry (λ (x y) x)))
'(curry_2 (λ (x y) x))
]

@;Our implementation generates each @racket[curry_i] at compile-time by folding
@; over the interpreted domain.
This same technique can be used to implement generalized @racket[map] in
 languages without variable-arity polymorphism@~cite[stf-esop-2009].

@racketblock[
> (define (cite (p : String) (d : String))
    (printf "~a v. ~a, U.S.\n" p d))
> (define plaintiff*
    '("Rasul" "Chisholm"))
> (define defendant*
    '("Bush" "Georgia"))
> (map cite plaintiff* defendant*)
Rasul v. Bush, U.S.
Chisholm v. Georgia, U.S.
]

Leaving out an argument to @racket[printf] or passing an extra list
 when calling @racket[map] will raise an arity error during elaboration.
On the other hand, if we modified @racket[cite] to take a third argument
 then the above call to @racket[map] would fail to compile.


@; =============================================================================
@section{Numeric Constants}

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
@section[#:tag "sec:sql"]{Database Queries}
@; db
@; TODO Ocaml, Haskell story
@; TODO connect to ew-haskell-2012 os-icfp-2008

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
