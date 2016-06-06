#lang scribble/sigplan @onecolumn

@; TODO
@; - revoke the AbsInt strategy
@; - work out the "ICFP style" correctness
@; - use objects in Haskell / ML for regexp (it's the only subtyping case?)
@;   ML row polymorphism
@;   Haskell ... ?
@; - 

@require["common.rkt"]

@title[#:tag "sec:correctness"]{Making Sense}

The thesis of this paper is that syntax extensions are an effective
 tool for refining an existing type system.
Ultimately we aim to show that given a fixed program and fixed type system,
 the type system will be able to correctly reject more programs that
 produce an error at runtime and correctly accept more programs that it
 could not previously express @emph{if} a layer of syntax extensions may be
 inserted between parsing and typechecking a program.
If library authors are permitted to write such extensions, then the language's
 type theory will become amenable to domain-specific typing rules without
 changes to the core language.

To make our goal precise, we define a simple, low-level programming language in @todo{Figure-1}.
On top of this untyped core language we impose the polymorphic type system
 defined in @todo{Figure-2}.
The idea is that user code is validated by the type system before it is compiled
 to the untyped language and run.
Um, we take the untyped language in @todo{Figure-1} as an abstract machine code.

Consider again the function @racket[vector-ref].
In @Secref{sec:background} we claimed that a type system could approximate the
 specification of @racket[vector-ref] in order to statically detect bugs.
We formalize this claim by interpreting functions as relations between their
 input and output values.

Start with the untyped function @exact{$\RktMeta{vector-ref}_\lambda$}.
For vectors @exact|{$\langle \RktMeta{v}_0 \ldots \RktMeta{v}_n \rangle$}| and
 indices @exact|{$0 \leq i < n$}|, @racket[vector-ref] returns @exact|{$\RktMeta{v}_i$}|.
On all other inputs, @racket[vector-ref] returns @|bot|.

  @exact|{
    \[ \RktMeta{vector-ref} = \big\{ (\RktMeta{v}, \RktMeta{i}, \RktMeta{v}_i)
                                    \mid \RktMeta{vector? v}
                                         ~~
                                         \RktMeta{natural? i}
                                         ~~
                                         \RktMeta{i} < \RktMeta{n}
                                 \big\} \]
  }|

The typed version of @exact|{$@RktMeta{vector-ref}$}| understands parametericity and
 drop side conditions

  @exact|{
    \[ \RktMeta{vector-ref} = \big\{ (\RktMeta{v}, \RktMeta{i}, \RktMeta{v}_i)
                                    \mid \RktMeta{vector? v}
                                         ~~
                                         \RktMeta{natural? i}
                                 \big\} \]
  }|

Sound overapprox because typed is superert

@; @; Programming language:
@; @;   vectors strings integers booleans
@; @;   lambdas
@; @;   map printf regexp-match
@; 
@; Types provide sound overapproximation
@; 
@; our goal: refine approximation
@; 
@; type soundness is separate, we inherit soundness as corrollary of 
@;  ourtype <= origtype
@; 


@section{Implementation}
The transformations described in this section have been implemented as a Racket package.
Our @emph{expand} metafunction is implemented by the Racket macro expander.
Each transformation is a macro wrapping a standard library function.
It's good.

Proofs are mechanized as unit tests within the package.
Readers are invited to critique our proofs by downloading and using the package.


@section{Comments}
Maybe you're wondering why we didn't prove contextual equivalence of our transformations.
A few reasons:
@itemlist[
  @item{
    Contextual equivalence is @emph{hard}, we don't expect library writers
    to do full proofs of that for every transformation they make.
  }
  @item{
    Our @emph{expand} metafunction is very simple, but in realistic languages
    it will be only part of a comprehensive syntax extension system.
    As Wand proved way back when, contextual equivalence for languages with
    syntax extensions is trivial in the sense that only syntactically equivalent
    terms are equal.

    Given a two terms @racket[e_1] and @racket[e_2], here is a Racket
     syntax extension which that distinguishes them.
    @racketblock[
      (define-syntax (distinguisher stx)
        (syntax-parse stx
         [(_ e_1 e_2)
          (if (equal? (syntax->datum #'e_1)
                      (syntax->datum #'e_2))
            #'((λ (x) (x x)) (λ (x) (x x)))
            #'(λ (x) x))]))
    ]
    For non-identical terms, @racket[(distinguisher e_1 ???)] is the context
     @racket[C] that separates them.
    Specifically @racket[(C e_1)] will diverge and @racket[(C e_2)] will not.
  }
]
