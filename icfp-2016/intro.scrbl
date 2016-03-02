#lang scribble/sigplan

@require["common.rkt"]


@title{Introduction}
@; format/printf : played out
@; regexp-match  : complicated, from HM
@; +             : way too simple
@; vector-length : boring

Suppose @racket[regexp-match] is a function for matching a
 regular expression pattern against a string value.
If the match succeeds then the function returns a list containing the
 part of the string that was matched and all matched groups
 corresponding to parenthesized sub-patterns of the regular expression.
Otherwise, it returns a value indicating the match failed.
@; TODO Specify the type system?
Suppose also that @racket[pattern] and @racket[group] are variables
 bound to string values at run-time.
Given these premises, what is the type of this expression?

@racketblock[
  (regexp-match pattern str)
]

One simple answer is @racket[(Option (Listof String))].
If all goes well the resulting value will be a list of strings (of unspecified
 length), but we know ahead of time that the match may fail.

In a programming language with the full power of dependent types, we can encode
 the relationship between @racket[pattern] and @racket[str] and give a very
 specific type relating the number of groups in @racket[pattern] to the number
 of elements in the result list.
Dependent types, however, typically require strategic use of type annotations
 and carefully-selected data representations.
One relatively simple type for the expression above is
 @racket[(Option (List[N] String))] where @racket[N] is the number of groups
 in @racket[pattern], but this requires the type @racket[List] to carry length
 information and a type for regular expressions that counts groups.
Building the infrastructure necessary to express that particular type of
 the call to @racket[regexp-match] may give more trouble than relief.

This paper explores a solution between simple and dependent types that
 should apply in any typed language with sufficiently powerful
 @emph{syntax extensions}, or macros.
The key idea is to refine types based on values apparent in the program text.
When the parameters to @racket[regexp-match] are bound at run-time, we
 conservatively assign the type @racket[(Option (Listof String))].
But if there is more information at hand, we use it.
For instance, both these expressions have the type
 @racket[(Option (List String String))] in our system.@note{Where @racket[List]
  is Typed Racket's type for heterogenous, sized lists; akin to tuple types in ML.}

@racketblock[
    (regexp-match "hello(o*)" str)

    (let ([pattern "hello(o*)"])
      (regexp-match pattern str))
]

The pattern here has exactly one group, delimited by the parentheses.
Thus if the match succeeds we get a list of two elements: the first being
 the entire matched substring and the second a string matching the regular
 expression @racket{o*}.
All this is clear from the program text to the programmer; our contribution
 is parsing the relevant information and forwarding it to the type checker.

@;Additionally replacing @racket[str] with a value gives an even more precise type by
@; evaluating the expression while type checking.


@section{Prior & Current Work}

A macro system provides a general framework for transforming and analyzing
 program syntax.
Languages with strong macro systems are surprisingly expressive@~cite[f-scp-1991].
Case in point, Herman and Meunier demonstrated how macros can propogate
 information embedded in string values and syntax patterns to a
 static analyzer@~cite[hm-icfp-2004].
Their illustrative examples were format strings, regular expression patterns,
 and database queries.
We adapt these examples to a typed programming language
 and give additional examples inspired by the literature on dependent types.
By inserting type annotations and boolean guards our macros indirectly
 facilitate type-checking.
Quite often---but not always---the inferred types can remove the need for
 run-time assertions.

Contents:
@itemlist[
  @item{A language-agnostic framework for value-dependent macros (Section 2).}
  @item{Diverse motivating examples, from a type-safe @racket[printf] to a basic typeclass system (Section 3).}
  @item{Description and evaluation of a Typed Racket implementation (Section 4).}
  @item{Correctness requirements for transformations (Section 5).}
  @item{Related work and conclusion (Section 6).}
]
