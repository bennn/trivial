#lang scribble/sigplan @onecolumn


@require["common.rkt"]


@title[#:tag "sec:intro"]{The Spirit and Letter of the Law}

Well-typed programs @emph{do} go wrong.
All the time, in fact:

@exact|{
\begin{SCodeFlow}\begin{RktBlk}\begin{SingleColumn}\RktSym{{\Stttextmore}}\RktMeta{}\mbox{\hphantom{\Scribtexttt{x}}}\RktMeta{}\RktPn{(}\RktSym{vector{-}ref}\RktMeta{}\mbox{\hphantom{\Scribtexttt{x}}}\RktMeta{}\RktPn{(}\RktSym{make{-}vector}\RktMeta{}\mbox{\hphantom{\Scribtexttt{x}}}\RktMeta{}\RktVal{2}\RktPn{)}\RktMeta{}\mbox{\hphantom{\Scribtexttt{x}}}\RktMeta{}\RktVal{3}\RktPn{)}\RktMeta{}

\RktMeta{}\RktSym{=={\Stttextmore}}\RktMeta{}\mbox{\hphantom{\Scribtexttt{x}}}\RktErr{vector-ref: index is out of range}


\RktMeta{}\RktSym{{\Stttextmore}}\RktMeta{}\mbox{\hphantom{\Scribtexttt{x}}}\RktMeta{}\RktPn{(}\RktSym{/}\RktMeta{}\mbox{\hphantom{\Scribtexttt{x}}}\RktMeta{}\RktVal{1}\RktMeta{}\mbox{\hphantom{\Scribtexttt{x}}}\RktMeta{}\RktVal{0}\RktPn{)}\RktMeta{}

\RktMeta{}\RktSym{=={\Stttextmore}}\RktMeta{}\mbox{\hphantom{\Scribtexttt{x}}}\RktErr{/: division by zero}

\RktMeta{}\RktSym{{\Stttextmore}}\RktMeta{}\mbox{\hphantom{\Scribtexttt{x}}}\RktMeta{}\RktPn{(}\RktSym{printf}\RktMeta{}\mbox{\hphantom{\Scribtexttt{x}}}\RktMeta{}\RktVal{"$\sim$s"}\RktPn{)}\RktMeta{}

\RktMeta{}\RktSym{=={\Stttextmore}}\RktMeta{}\mbox{\hphantom{\Scribtexttt{x}}}\RktErr{printf: format string requires 1 argument}

\end{SingleColumn}\end{RktBlk}\end{SCodeFlow}
}|

Of course, Milner's catchphrase was about preventing type errors.
The above are all @emph{value errors} that depend on properties not expressed
 by Typed Racket's standard vector, integer, and string datatypes.
Even so, it is clear to the programmer that the expressions will go wrong.

Likewise, there are useful functions that many type systems cannot express.
Simple examples include a @racket[first] function for tuples of arbitrary size
 and a @racket[curry] function for procedures that consume such tuples.
The standard work-around@~cite[fi-jfp-2000] is to maintain size-indexed
 families of functions to handle the common cases, for instance:

@codeblock{
    > (define (curry_3 f)
        (λ (x) (λ (y) (λ (z) (f x y z)))))
}
@;    Prelude> let first_3 (x, y, z) = x

These problems are well known, and are often used to motivate research on
 dependently typed programming languages@~cite[a-icfp-1999].
Short of abandoning ship for a completely new type system, languages including
 Haskell, Java, OCaml, and Typed Racket have seen proposals for detecting
 certain values errors or expressing the polymorphism in functions
 such as @racket[curry] with few (if any) changes to the existing type system@~cite[ddems-icse-2011 ew-haskell-2012 ks-plpv-2006 kkt-pldi-2016 lb-sigplan-2014].
What stands between these proposals and their adoption is the complexity or
 annotation burden they impose on language users.

This pearl describes a low-complexity, annotation-free (@Secref{sec:experience})
 technique for detecting
 value errors and expressing polymorphism over values.
The key is to run a @emph{textualist}@note{A textualist interprets laws by
   reading exactly the words on the page rather that by guessing the words' intended meaning.}
 elaboration over programs before type-checking and propagate value information
 evident from the program syntax to the type checker.
In terms of the first example in this section, our elaborator infers that the
 @racket[vector-ref] at position @racket[3] is out of bounds because it knows that
 @racket[(make-vector n)]
 constructs a vector with @racket[n] elements.

Our implementation is a Typed Racket library that shadows functions
 such as @racket[make-vector] with textualist elaborators following the
 guidelines stated in @Secref{sec:solution}.
We make essential use of Racket's macro system@~cite[fcdb-jfp-2012]
 to reason locally, associate inferred data with bound identifiers, and
 cooperate with the rules of lexical scope.
For the adventurous reader, @Secref{sec:implementation} describes the
 main services provided by Racket's macros.
Nevertheless, Typed Clojure@~cite[clojure-macros],
 Rust@~cite[rust-compiler-plugins], and Scala@~cite[ramho-hosc-2013]
 could implement our approach just as well.

For a sense of the practical use-cases we envision, consider the function
 @racket[regexp-match], which matches a regular expression pattern against
 a string and returns either a list of matched substrings or @racket[#false]
 if the match failed.

@racketblock[
> (regexp-match #rx"(.*) v\\. (.*),"
    "Morrison v. Olson, 487 U.S. 654")
'("Morrison v. Olson" "Morrison" "Olson")
]

The parentheses in the regular expression delimit groups to match and
 return.
In this example, there are two such groups.
We have written an elaborator for @racket[regexp-match] that will statically
 parse its first argument, count groups, and refine the
 result type of specific calls to @racket[regexp-match].
The elaborator also handles the common case where the regular expression
 argument is a compile-time constant and respects @exact{$\alpha$}-equivalence.

Whereas Typed Racket will raise a type error on the following code because
 it cannot be sure @racket[second] will produce a string,
 importing our library convinces Typed Racket that the code will succeed.

@codeblock{
(define case-regexp #rx"(.*) v\\. (.*),")
(define rx-match regexp-match)

(define (get-plaintiff (s : String)) : String
  (cond
   [(rx-match case-regexp s)
    => second]
   [else "J. Doe"]))
}

@Secref{sec:usage} has more examples.
@Secref{sec:conclusion} concludes.

@; =============================================================================

@parag{Lineage}

Our technique builds on the approach of Herman and Meunier, who first
 demonstrated how Racket macros can propagate
 data embedded in string values and syntax patterns to a
 static analyzer@~cite[hm-icfp-2004].
Their illustrative examples were format strings, regular expressions,
 and database queries.
Relative to their pioneering work, we adapt Herman & Meunier's
 transformations to a typed language, suggest new applications, and
 describe how to compose the results of analyses.

