#lang scribble/sigplan
@(require "common.rkt" racket/string racket/sandbox)

@; TODO clever examples

@title[#:tag "sec:usage"]{What we can Learn from Values}

We have defined useful elaborators for a variety of
 common programming tasks ranging from type-safe string formatting to
 constant-folding of arithmetic.
These elaborators are implemented in Typed Racket@~cite[TypedRacket], a macro-extensible
 typed language that compiles into Racket@~cite[plt-tr1].
An important component of Typed Racket's design is that all macros in a program
 are fully expanded before type-checking begins.
This protocol lets us implement our elaborators as macros that expand into
 typed code.

Each elaborator is defined as a @emph{local} transformation on syntax.
Code produced by an elaboration may be associated with compile-time values
 for other elaborators to access.
Using these values, we support variable bindings and propagate information upward through
 nested elaborations.

@parag{Conventions}
@itemlist[
 @item{
Interpretation and elaboration functions are defined over symbolic expressions
 and values; specifically, over @emph{syntax objects}.
To distinguish terms and syntax objects,
 we quote the latter and typeset it in green.
Hence @racket[(λ (x) x)] is the identity function
 and @racket['(λ (x) x)] is a syntax object.
@; TODO @todo{No longer accurate, not quoting was prettier}
} @item{
Values are typeset in @exact|{\RktVal{green}}| because their syntax and
 term representations are identical.
} @item{
Syntax objects carry lexical information, but our examples treat them as
 flat symbols.
} @item{
We use an infix @tt{::} to write explicit type annotations and casts,
 for instance @racket[(x :: Integer)].
Annotations and casts normally have two different syntaxes, respectively
 @racket[(ann x Integer)] and @racket[(cast x Integer)].
}
]

@; =============================================================================
@section{Format Strings}

@; TODO add note about the ridiculous survey figure? Something like 4/5 doctors
@; TODO note regexp is the first?
Format strings are the world's second most-loved domain-specific language.
@; @~cite[wmpk-algol-1968]
All strings are valid format strings; additionally, a format string may contain
 @emph{format directives} describing @emph{where} and @emph{how} values can be
 spliced into the format string.
@; TODO scheme or common lisp?
Racket follows the Lisp tradition of using a tilde character (@tt{~})
 to prefix format directives.
For example, @racket[~s] converts any value to a string and @racket[~b] converts a
 number to binary form.

@exact|{
\begin{SCodeFlow}\begin{RktBlk}\begin{SingleColumn}\Scribtexttt{{\Stttextmore} }\RktPn{(}\RktSym{printf}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{"binary($\sim$s) = $\sim$b"}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{7}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{7}\RktPn{)}

\evalsto\RktOut{binary(7) = 111}

\begin{SingleColumn}\end{SingleColumn}\end{SingleColumn}\end{RktBlk}\end{SCodeFlow}
}|

If the format directives do not match the arguments to @racket[printf], most
 languages fail at run-time.
This is a simple kind of value error that could be caught statically.

@exact|{
\begin{SCodeFlow}\begin{RktBlk}\begin{SingleColumn}\Scribtexttt{{\Stttextmore} }\RktPn{(}\RktSym{printf}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{"binary($\sim$s) = $\sim$b"}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{"7"}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{"7"}\RktPn{)}

\evalsto\RktErr{printf: format string requires an exact{-}number}

\begin{SingleColumn}\end{SingleColumn}\end{SingleColumn}\end{RktBlk}\end{SCodeFlow}
}|

Detecting inconsistencies between a format string and its arguments is straightforward
 if we define an interpretation @racket[fmt->types] for
 reading types from a format string value.
In Typed Racket this function is rather simple because the most common
 directives accept @code{Any} type of value.

@exact|{
\hfill\fbox{\RktMeta{fmt->types} $\in \interp$}
\vspace{-4mm}
}|
@racketblock[
> (fmt->types "binary(~s) = ~b")
==> [Any Integer]
> (fmt->types '(λ (x) x))
==> #false
]

Now to use @racket[fmt->types] in an elaboration.
Given a call to @racket[printf], we check the number of arguments and
 add type annotations using the inferred types.
For all other syntax patterns, we perform the identity elaboration.

@exact|{
\hfill\fbox{$\elabf \in \elab$}
\vspace{-4mm}
}|
@racketblock[
> ⟦(printf "~s")⟧
==> ⊥
> ⟦(printf "~b" 2)⟧
==> (printf "~b" (2 :: Integer))
> ⟦printf ⟧
==> printf
]

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
> (regexp-match #rx"-(2*)-" "111-222-3333")
==> '("-222-" "222")
> (regexp-match #rx"¥(.*)" "$2,000")
==> #false
]

The first argument to @racket[regexp-match] is a regular expression pattern.
Inside the pattern, the parentheses delimit sub-pattern @emph{groups}, the dots match
 any single character, and the Kleene star matches zero-or-more repetitions
 of the pattern-or-character preceding it.
The second argument is a string to match against the pattern.
If the match succeeds, the result is a list containing the entire matched string
 and substrings corresponding to each group captured by a sub-pattern.
If the match fails, @racket[regexp-match] returns @racket[#false].

Groups may fail to capture even when the overall match succeeds.
This can happen when a group is followed by a Kleene star.

@racketblock[
> (regexp-match #rx"(a)*(b)" "b")
==> '("b" #f "b")
]

Therefore, a catch-all type for @racket[regexp-match] is fairly large:
 @racket[(Regexp String -> (U #f (Listof (U #f String))))].
Using this general type is cumbersome for simple patterns
 where a match implies that all groups will successfully capture.

@racketblock[
> (define (get-domain (email : String)) : String
    (cond
     [(regexp-match #rx"(.*)@(.*)" email)
      => third]
     [else "Match Failed"]))
]

@exact|{
\vspace{-1mm}%
$\!\!$\evalsto\RktErr{Type Error: expected String, got (U \#false String)}%
\vspace{1mm}
}|


We alleviate the need for casts and guards in simple patterns
 with a parentheses-counting
 interpretation that parses regular expressions
 and returns the number of groups.
If there is any doubt whether a group will capture, we default to the general
 @racket[regexp-match] type.

@exact|{
\hfill\fbox{$\RktMeta{rx->groups} \in \interp$}
\vspace{-4mm}
}|
@racketblock[
> (rx->groups #rx"(a)(b)(c)")
==> 3
> (rx->groups #rx"((a)b)")
==> 2
> (rx->groups #rx"(a)*(b)")
==> #false
]

The corresponding elaboration
 inserts casts to subtype the result of calls to @racket[regexp-match].
It also raises syntax errors when an uncompiled regular expression contains
 unmatched parentheses.

@exact|{
\vspace{1mm}
\hfill\fbox{$\elabf \in \elab$}
\vspace{-4mm}
}|
@racketblock[
> ⟦(regexp-match #rx"(a)b" str)⟧
==> ((regexp-match #rx"(a)b" str)
      :: (U #f (List String String)))
> ⟦(regexp-match "(" str)⟧
==> ⊥
]


@; =============================================================================
@section{Anonymous Functions}

By tokenizing symbolic λ-expressions, we can statically infer their domain.

@exact|{
\hfill\fbox{$\RktMeta{fun->domain} \in \interp$}
\vspace{-4mm}
}|
@racketblock[
> (fun->domain (λ (x y z)
                 (x (z y) y)))
==> [Any Any Any]
> (fun->domain (λ ([x : Real] [y : Real])
                  x))
==> [Real Real]
]

When domain information is available at calls to a @racket[curry] function,
 we elaborate to a type-correct, indexed version of @racket[curry].
Conceptually, we give @racket[curry] the unusable type @racket[(⊥ -> ⊤)] and
 elaboration produces a subtype @racket[curry_i].

@exact|{
\hfill\fbox{$\elabf \in \elab$}
\vspace{-4mm}
}|
@racketblock[
> ⟦(curry (λ (x y) x))⟧
==> (curry_2 (λ (x y) x))
]

This same technique can be used to implement generalized @racket[map] in
 languages without variable-arity polymorphism@~cite[stf-esop-2009].

@racketblock[
> (define (cite (p : String) (d : String))
    (printf "~s v. ~s, U.S.\n" p d))
> (define plaintiff*
    '("Rasul" "Chisholm"))
> (define defendant*
    '("Bush" "Georgia"))
> (map cite plaintiff* defendant*)
]

@exact|{
\vspace{-1mm}%
$\!\!$\evalsto\RktOut{Rasul v. Bush, U.S.}\\
$\hphantom{xxxxx}$\RktOut{Chisholm v. Georgia, U.S.}
\vspace{1mm}
}|


Leaving out an argument to @racket[printf] or passing an extra list
 when calling @racket[map] will raise an arity error during elaboration.
On the other hand, if we modified @racket[cite] to take a third argument
 then the above call to @racket[map] would raise a type error.


@; =============================================================================
@section{Numeric Constants}

The identity interpretation @exact{$\RktMeta{id} \in \interp$}
 lifts values to the elaboration environment.
When composed with a filter, we can recognize types of compile-time constants.

@exact|{
\hfill\fbox{$\RktMeta{int?} \circ \RktMeta{id} = \RktMeta{int?} \in \interp$}
\vspace{-4mm}
}|
@racketblock[
> (int? 2)
==> 2
> (int? "~s")
==> #false
]

Constant-folding versions of arithmetic operators are now easy to define
 in terms of the built-in operations.
Our implementation re-uses a single fold/elaborate loop to make
 textualist wrappers over @racket[+], @racket[expt] and others.

@exact|{
\vspace{2mm}
\hfill\fbox{$\elabf \in \elab$}
\vspace{-4mm}
}|
@racketblock[
> (define a ⟦3⟧)
> (define b ⟦(/ 1 (- a a))⟧)
]

@exact|{
\vspace{-1mm}%
$\!\!$\evalsto\RktErr{Error: division by zero}%
\vspace{1mm}
}|

Partial folds also work as expected.

@racketblock[
> (* 2 3 7 z)
==> (* 42 z)
]

Taken alone, this re-implementation of constant folding in an earlier compiler
 stage is not very exciting.
But since folded expressions propagate their result upwards to arbitrary
 analyses, we can combine these elaborations with a size-aware vector library to
 guard against index errors at computed locations.


@; =============================================================================
@section[#:tag "sec:vector"]{Fixed-Length Structures}

Fixed-length data structures are often initialized with a constant or
 computed-constant length.
Racket's vectors are one such structure.
For each built-in vector constructor, we thus define an interpretation:

@exact|{
\vspace{2mm}
\hfill\fbox{$\RktMeta{vector->size} \in \interp$}
\vspace{-4mm}
}|
@racketblock[
> (vector->size '#(0 1 2))
==> 3
> (vector->size '(make-vector 100))
==> 100
> (vector->size '(make-vector (/ 12 3)))
==> 4
]

After interpreting, we associate the size with the new vector at compile-time.
Other elaborators can use and propagate these sizes; for instance, we have
 implemented elaborating layers for 13 standard vector operations.
Together, they constitute a length-aware vector library that serves as a
 drop-in replacement for existing code.
If size information is ever missing, the operators silently default
 to Typed Racket's behavior.

@exact|{
\vspace{2mm}
\hfill\fbox{$\elabf \in \elab$}
\vspace{-4mm}
}|
@racketblock[
> ⟦(vector-ref (make-vector 3) (+ 2 2))⟧
==> ⊥
> ⟦(vector-length (vector-append '#(A B) '#(C D)))⟧
==> 4
> ⟦(vector-ref (vector-map add1 '#(3 3 3)) 0)⟧
==> (unsafe-ref (vector-map add1 '#(3 3 3)) 0)
]

For the most part, these elaborations simply manage sizes and
 delegate the main work to Typed Racket vector operations.
We do, however, optimize vector references to unsafe primitives and
 specialize operations like @racket[vector-length], as shown above.


@; =============================================================================
@section[#:tag "sec:sql"]{Database Schema}

Racket's @racket[db] library provides a string-based API for executing @tt{sql}
 queries.@note{In this section, we use @tt{sql} as an abbreviation for @tt{postgresql}.}
After connecting to a database, @tt{sql} queries embedded in strings can be run
 to retrieve row values, represented as Racket vectors.
Queries may optionally contain @emph{query parameters}---natural numbers
 prefixed by a dollar sign (@tt{$}).
Arguments substituted for query parameters are guarded against @tt{sql} injection.

@exact|{
\begin{SCodeFlow}\begin{RktBlk}\begin{SingleColumn}\RktSym{{\Stttextmore}}\mbox{\hphantom{\Scribtexttt{x}}}\RktPn{(}\RktSym{define}\mbox{\hphantom{\Scribtexttt{x}}}\RktSym{C}\mbox{\hphantom{\Scribtexttt{x}}}\RktPn{(}\RktSym{sql{-}connect}\mbox{\hphantom{\Scribtexttt{x}}}\RktPn{\#{\hbox{\texttt{:}}}user}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{"admin"}

\mbox{\hphantom{\Scribtexttt{xxxxxxxxxxxxxxxxxxxxxxxxx}}}\RktPn{\#{\hbox{\texttt{:}}}database}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{"SCOTUS"}\RktPn{)}\RktPn{)}

\RktSym{{\Stttextmore}}\mbox{\hphantom{\Scribtexttt{x}}}\RktPn{(}\RktSym{query{-}row}\mbox{\hphantom{\Scribtexttt{x}}}\RktSym{C}

\mbox{\hphantom{\Scribtexttt{xxxx}}}\RktVal{"SELECT plaintiff FROM rulings}\\
\mbox{\hphantom{\Scribtexttt{xxxxx}}}\RktVal{WHERE name = {\textquotesingle}\$1{\textquotesingle} LIMIT 1"}

\mbox{\hphantom{\Scribtexttt{xxxx}}}\RktVal{2001}\RktPn{)}

\evalsto\RktVal{\#}\RktVal{(}\RktVal{"Kyllo"}\RktVal{)}\end{SingleColumn}\end{RktBlk}\end{SCodeFlow}
}|

This is a far cry from language-integrated query@~cite[mbb-sigmod-2006] or
 Scala's LMS@~cite[ra-icfp-2015], but the interface
 is relatively safe and very simple to use.

Typed Racket programmers may use the @racket[db] library by assigning
 type signatures to functions like @racket[query-row].
This is somewhat tedious, as the distinct operations for querying one value,
 one row, or a lazy sequence of rows need similar types.
A proper type signature might express the database library as a functor over the
 database schema, but Typed Racket does not have functors or even existential types.
Even if it did, the queries-as-strings interface makes it impossible for a standard
 type checker to infer type constraints on query parameters.

The situation worsens if the programmer uses multiple database connections.
One can either alias one query function to multiple identifiers (each with a specific type)
 or weaken type signatures and manually type-cast query results.

@subsection{Phantom Types to the Rescue}
By associating a database schema with each connection, our elaboration technique
 can provide a uniform solution to these issues.
We specialize both the input and output of calls to @racket[query-row],
 helping catch bugs as early as possible.

Our solution, however, is not entirely annotation-free.
We need a schema representing the target database; for this, we ask
 the programmer to supply an S-expression of symbols and types
 that passes a @racket[schema?] predicate.
This approach is similar to phantom types@~cite[lm-dsl-1999].

@exact|{
\vspace{1mm}
\hfill\fbox{$\RktMeta{schema?} \in \interp$}
\vspace{-4mm}
}|
@racketblock[
> (define scotus-schema
    '([decisions [(id . Natural)
                  (plaintiff . String)
                  (defendant . String)
                  (year . Natural)]]))
> (schema? scotus-schema)
==> '([decisions ....])
]

The above schema represents a database with at least one table, called @racket[decisions],
 which contains at least 4 rows with the specified names and types.
In general a schema may specify any number of tables and rows.
We statically disallow access to unspecified ones in our elaborated query operations.

In addition to the @exact{$\RktMeta{schema?}$} predicate, we define one more
 interpretation and two elaborations.
The first elaboration is for connecting to a database.
We require a statically-known schema object and elaborate to a normal connection.

@exact|{
\vspace{1mm}
\hfill\fbox{$\elabf \in \elab$}
\vspace{-4mm}
}|
@racketblock[
> ⟦(sql-connect #:user "admin"
                #:database "SCOTUS")⟧
==> ⊥
> ⟦(sql-connect scotus-schema
                #:user "admin"
                #:database "SCOTUS")⟧
==> (sql-connect #:user "admin"
                  #:database "SCOTUS")
]

The next interpretation and elaboration are for reading constraints from
 query strings.
We parse @tt{SELECT} statements using @racket[sql->constr] and extract
@itemlist[
  @item{the names of selected columns,}
  @item{the table name, and}
  @item{an association from query parameters to column names.}
]

@exact|{
\vspace{1mm}
\hfill\fbox{$\RktMeta{sql->constr} \in \interp$}
\vspace{-4mm}

\begin{SCodeFlow}\begin{RktBlk}\begin{SingleColumn}
\RktSym{{\Stttextmore}}\mbox{\hphantom{\Scribtexttt{x}}}\RktMeta{(sql->constr}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{"SELECT * FROM loans"}\RktMeta{)}

\RktSym{=={\Stttextmore}}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{{\textquotesingle}}\RktVal{[}\RktVal{*}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{decisions}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{(}\RktVal{)}\RktVal{]}\end{SingleColumn}\end{RktBlk}\end{SCodeFlow}

\RktSym{{\Stttextmore}}\mbox{\hphantom{\Scribtexttt{x}}}\RktMeta{(sql->constr}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{"SELECT defendant FROM decisions}\\
\mbox{\hphantom{\Scribtexttt{xxxxxxxxxxxxxxxxxxx}}}\RktVal{WHERE plaintiff = {\textquotesingle}\$1{\textquotesingle}"}\RktMeta{)}

\RktSym{=={\Stttextmore}}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{{\textquotesingle}}\RktVal{[}\RktVal{(}\RktVal{defendant}\RktVal{)}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{decisions}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{(}\RktVal{\$1}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{{\hbox{\texttt{.}}} }\RktVal{plaintiff}\RktVal{)}\RktVal{]}
}|

The schema, connection, and query constraints now come together in elaborations
 such as a wrapper @exact{$\elabf$} over @racket[query-row].
There is still a non-trivial amount of work to be done resolving wildcards and
 validating row names before the type-annotated result is produced,
 but all the necessary information is available, statically.

@exact|{
\vspace{1mm}
\hfill\fbox{$\elabf \in \elab$}
\vspace{-4mm}
\begin{SCodeFlow}\begin{RktBlk}\begin{SingleColumn}\RktSym{{\Stttextmore}}\mbox{\hphantom{\Scribtexttt{x}}}\RktSym{$[\![$}\RktMeta{(}\RktMeta{query{-}row}\mbox{\hphantom{\Scribtexttt{x}}}\RktMeta{C}

\mbox{\hphantom{\Scribtexttt{xxxxxx}}}\RktVal{"SELECT plaintiff FROM decisions}\\
\mbox{\hphantom{\Scribtexttt{xxxxxxx}}}\RktVal{WHERE year = {\textquotesingle}\$1{\textquotesingle} LIMIT 1"}

\mbox{\hphantom{\Scribtexttt{xxxxx}}}\RktMeta{2006}\RktMeta{)}\RktSym{$]\!]$}

\RktSym{=={\Stttextmore}}\mbox{\hphantom{\Scribtexttt{x}}}\RktMeta{(}\RktMeta{(}\RktMeta{query{-}row}\mbox{\hphantom{\Scribtexttt{x}}}\RktMeta{C}

\mbox{\hphantom{\Scribtexttt{xxxxxxxx}}}\RktVal{"SELECT {\hbox{\texttt{.}}}{\hbox{\texttt{.}}}{\hbox{\texttt{.}}}{\hbox{\texttt{.}}}"}

\mbox{\hphantom{\Scribtexttt{xxxxxxxx}}}\RktMeta{(}\RktMeta{2006}\mbox{\hphantom{\Scribtexttt{x}}}\RktMeta{{\hbox{\texttt{:}}}}\mbox{\hphantom{\Scribtexttt{x}}}\RktMeta{Natural}\RktMeta{)}\RktMeta{)}

\mbox{\hphantom{\Scribtexttt{xxxxxx}}}\RktMeta{{\hbox{\texttt{:}}}{\hbox{\texttt{:}}}}\mbox{\hphantom{\Scribtexttt{xxx}}}\RktMeta{(}\RktMeta{Vector}\mbox{\hphantom{\Scribtexttt{x}}}\RktMeta{String}\RktMeta{)}\RktMeta{)}
\\
\\
\RktSym{{\Stttextmore}}\mbox{\hphantom{\Scribtexttt{x}}}\RktSym{$[\![$}\RktMeta{(}\RktMeta{query{-}row}\mbox{\hphantom{\Scribtexttt{x}}}\RktMeta{C}

\mbox{\hphantom{\Scribtexttt{xxxxxx}}}\RktVal{"SELECT * FROM decisions}\\
\mbox{\hphantom{\Scribtexttt{xxxxxxx}}}\RktVal{WHERE plaintiff = {\textquotesingle}\$1{\textquotesingle} LIMIT 1"}

\mbox{\hphantom{\Scribtexttt{xxxxxx}}}\RktVal{"United States"}\RktMeta{)}\RktSym{$]\!]$}

\RktSym{=={\Stttextmore}}\mbox{\hphantom{\Scribtexttt{x}}}\RktMeta{(}\RktMeta{(}\RktMeta{query{-}row}\mbox{\hphantom{\Scribtexttt{x}}}\RktMeta{C}

\mbox{\hphantom{\Scribtexttt{xxxxxxxx}}}\RktVal{"SELECT {\hbox{\texttt{.}}}{\hbox{\texttt{.}}}{\hbox{\texttt{.}}}"}

\mbox{\hphantom{\Scribtexttt{xxxxxxxx}}}\RktMeta{(}\RktVal{"United States"}\mbox{\hphantom{\Scribtexttt{x}}}\RktMeta{{\hbox{\texttt{:}}}}\mbox{\hphantom{\Scribtexttt{x}}}\RktMeta{String}\RktMeta{)}\RktMeta{)}

\mbox{\hphantom{\Scribtexttt{xxxxxx}}}\RktMeta{{\hbox{\texttt{:}}}{\hbox{\texttt{:}}}}\mbox{\hphantom{\Scribtexttt{xxx}}}\RktMeta{(}\RktMeta{Vector}\mbox{\hphantom{\Scribtexttt{x}}}\RktMeta{Natural}\mbox{\hphantom{\Scribtexttt{x}}}\RktMeta{String}\mbox{\hphantom{\Scribtexttt{x}}}\RktMeta{String}\mbox{\hphantom{\Scribtexttt{x}}}\RktMeta{Natural}\RktMeta{)}\RktMeta{)}
\\
\\
\RktSym{{\Stttextmore}}\mbox{\hphantom{\Scribtexttt{x}}}\RktSym{$[\![$}\RktMeta{(}\RktMeta{query{-}row}\mbox{\hphantom{\Scribtexttt{x}}}\RktMeta{C}\mbox{\hphantom{\Scribtexttt{x}}}\RktVal{"SELECT foo FROM decisions"}\RktMeta{)}\RktSym{$]\!]$}

\RktSym{=={\Stttextmore}}\mbox{\hphantom{\Scribtexttt{x}}}\RktSym{$\perp$}\end{SingleColumn}\end{RktBlk}\end{SCodeFlow}
}|

Results produced by @racket[query-row] are vectors with a known length;
 as such they cooperate with our library of vector operations described in
 @Secref{sec:vector}.
Accessing a constant index into a row vector is statically guaranteed
 to be in bounds.

@; casts can fail, especially if you wildcard without saying all the rows

