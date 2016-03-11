#lang scribble/sigplan @onecolumn

@; Two things going on:
@; - attach & propogate type++ information at COMPILE time
@; -infer type++ info from value forms

@; TODO need a word for these 'observable properties'
@; - regexp groups
@; - format characters
@; - procedure arity
@; - tuple size
@; - vector length
@; - database schema

@; Tuples in Haskell http://hackage.haskell.org/package/tuple
@; Regexp
@; - ocaml http://caml.inria.fr/pub/docs/manual-ocaml/libref/Str.html
@; - haskell https://hackage.haskell.org/package/regex-compat/docs/Text-Regex.html

@require["common.rkt"]


@title[#:tag "sec:intro"]{The Spirit and Letter of the Law}
@; tautology checker
@; curry
@; deep
@; regexp match

Well-typed programs @emph{do} go wrong.
All the time, in fact:

@codeblock{
    Prelude> [0,1,2] !! 3
    *** Exception: Prelude.!!: index too large
    Prelude> 1 `div` 0
    *** Exception: divide by zero
    Prelude> import Text.Printf
    Prelude Text.Printf> printf "%d"
    *** Exception: printf: argument list ended prematurely
}

Of course, Milner's catchphrase was about preventing type errors.
The above are all @emph{value errors} that depend on properties not expressed
 by Haskell's standard list, integer, and string datatypes.
Even so, it is obvious to the programmer that the expressions will go wrong
 and there have been many proposals for detecting these and other value
 errors@~cite[a-icfp-1999 ddems-icse-2011 lb-sigplan-2014].
What stands between these proposals and their adoption is the complexity or
 annotation burden they impose on language users.

Likewise, there are useful functions that many type systems cannot express.
Simple examples include a @racket[first] function for tuples of arbitrary size
 and a @racket[curry] function for procedures that consume such tuples.
The standard work-around@~cite[fi-jfp-2000] is to write size-indexed families of functions to handle
 the common cases, for instance:

@codeblock{
    Prelude> let curry_3 f = \ x y z -> f (x,y,z)
}
@;    Prelude> let first_3 (x, y, z) = x

This pearl describes a technique for statically detecting value errors and
 statically generalizing value-indexed functions.
We catch all the above-mentioned wrong programs and offer a single implementation
 of @racket[curry] that obviates the need to copy/paste and manage size-indexed
 versions.
Furthermore, we demonstrate applications to regular expression matching,
 vectorized operations, and querying a database.

The key to our success--and also our weakness---is that we specialize
 procedure call sites based on compile-time constant values.
Run-time input foils the technique, but nonetheless we have found the idea useful
 for many common programming tasks.
Moreover, the approach may be implemented as a library and used as a drop-in
 fix for existing code.
Simply importing the library overrides standard procedures with specialized ones.
No further annotations are necessary; if specialization fails we default to
 the program's original behavior.
Put another way, our technique interprets the @emph{letter}
 of programs before the type system conducts its coarser, type-of-values analysis.
Like Shakespeare's Portia, we understand that the phrase ``pound of flesh''
 says nothing about drawing blood and specialize accordingly.
@; Like XXX laborers who did work-to-rule ...

Our implementation happens to be for Typed Racket, but
 Typed Clojure,
 Haskell,
 OCaml,
 Rust,
 and Scala
 would have been equally suitable hosts.
The main requirement is that the language provides a means of altering the syntax
 of a program before type checking.
Such tools are more formally known as @emph{macro} or @emph{syntax extension}
 systems.
At any rate, we sketch implementations for the five languages
 listed above in the conclusion.

Until that time when we must part, this pearl first describes our general
 approach in @Secref{sec:solution} and then illustrates the approach with
 specific examples in @Secref{sec:usage}.
We briefly report on practical experiences with our library
 in @Secref{sec:experience}.
Adventurous readers may enjoy learning about implementation details
 in @Secref{sec:implementation}, but everyone else is invited to skip to the
 end and try implementing a letter-of-values analysis in their language of choice.


@; =============================================================================

@parag{Lineage}

Herman and Meunier demonstrated how Racket macros can propagate
 information embedded in string values and syntax patterns to a
 static analyzer@~cite[hm-icfp-2004].
Their illustrative examples were format strings, regular expressions,
 and database queries.
Relative to their pioneering work, our contribution is adapting Herman & Meunier's
 transformations to a typed programming language.
By inserting type annotations and boolean guards, our transformations indirectly
 cooperate with the type checker without significantly changing the program's
 syntax.
We also give a modern description of Racket's macro system and handle definitions
 as well as in-line constants.


@parag{Eager Evaluation}

Our implementation is available as a Racket package.
To install the library, download Racket and then run @racket[raco pkg install ???].
The source code is on Github at: @url["https://github.com/???/???"].
Suggestions for a new package name are welcome.
