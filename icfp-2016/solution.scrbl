#lang scribble/sigplan @onecolumn

@require["common.rkt"]

@title[#:tag "sec:solution"]{Interpretations, Elaborations}

A textualist elaborator (henceforth, @emph{elaborator})
 is a specific kind of macro, meant to be run on the syntax of a program
 before the program is type-checked.
The behavior of an elaborator is split between two functions: interpretation
 and elaboration.

An @emph{interpretation} function attempts to parse data from an expression;
 for example parsing the number of groups from a regular expression string.
In the Lisp tradition, we will use the value @racket[#false] to indicate
 failure and refer to interpretation functions as @emph{predicates}.
Using @exact|{\RktMeta{expr}}| to denote the set of syntactically valid, symbolic
 program expressions
 and @exact|{\RktVal{val}}| to denote the set of symbolic values,
 we define the set @exact{$\interp$}
 of interpretation functions.

  @exact|{$$\interp\ : \big\{\RktMeta{expr} \rightarrow ({\RktVal{val}} \cup {\tt \RktVal{\#false}})\big\}$$}|

If @exact|{\RktMeta{f} $\in \interp$}| and @exact|{\RktMeta{e} $\in \RktMeta{expr}$}|,
 it may be useful to think of
 @exact|{\RktMeta{(f e)}}| as @emph{evidence} that the expression @exact|{\RktMeta{e}}|
 is recognized by @exact|{\RktMeta{f}}|.
Alternatively, @exact|{\RktMeta{(f e)}}| is a kind of interpolant@~cite[c-jsl-1997],
 representing data embedded in @exact|{\RktMeta{e}}| that justifies a certain
 program transformation.
Correct interpretation functions @exact|{\RktMeta{f}}| obey two guidelines:

@itemize[
  @item{
    The expressions for which @exact|{\RktMeta{f}}| returns a non-@racket[#false]
     value must have some common structure.
  }
  @item{
    Non-@racket[#false] results @exact|{\RktMeta{(f e)}}| are computed by a
     uniform algorithm and must have some common structure.
  }
]

This vague notion of common structure may be expressible as a type in an
 appropriate type system.
It is definitely not a type in the target language's type system.

Functions in the set @exact|{$\elab$}| of @emph{elaborations}
 map expressions to expressions, for instance replacing a call to @racket[curry]
 with a call to @racket[curry_3].
We write elaboration functions as @exact{$\elabf$} and their application
 to an expression @exact|{\RktMeta{e}}| as @exact|{$\llbracket$\RktMeta{e}$\rrbracket$}|.
Elaborations are allowed to fail raising syntax errors, which we notate as
 @exact|{$\bot$}|.

    @exact|{$$\elab : \big\{ {\RktMeta{expr}} \rightarrow ({\RktMeta{expr}} \cup \bot)\big\} $$}|

The correctness specification for an elaborator @exact{$\elabf \in \elab$}
 is defined in terms of the language's typing judgment @exact|{$~\vdash \RktMeta{e} : \tau$}|
 and evaluation relation @exact|{$\untyped{\RktMeta{e}} \Downarrow \RktVal{v}$}|.
The notation @exact|{$\untyped{\RktMeta{e}}$}| is the untyped erasure
 of @exact|{\RktMeta{e}}|.
We also assume a subtyping relation @exact|{$\subt$}| on types.
Let @exact|{$\elabfe{\RktMeta{e}} = \RktMeta{e'}$}|:

@itemlist[
  @item{@emph{
    If @exact|{$~\vdash \RktMeta{e} : \tau$}| and @exact|{$~\vdash \RktMeta{e'} : \tau'$}|
    @exact|{\\}|
    then @exact|{$\tau' \subt \tau$}|
    @exact|{\\}|
    and both
     @exact|{$\untyped{\RktMeta{e}} \Downarrow  \RktVal{v}$}| and
     @exact|{$\untyped{\RktMeta{e'}} \Downarrow \RktVal{v}$}|.
  }}

  @item{@emph{
    If @exact|{$~\not\vdash \RktMeta{e} : \tau$}| but @exact|{$~\vdash \RktMeta{e'} : \tau'$}|
     @exact|{\\}|
     then @exact|{$\untyped{\RktMeta{e}} \Downarrow \RktVal{v}$}| and
     @exact|{$\untyped{\RktMeta{e'}} \Downarrow \RktVal{v}$}|.
  }}

  @item{@emph{
    If @exact|{$~\vdash \RktMeta{e} : \tau$}| but @exact|{$\RktMeta{e'} = \bot$}|
     or @exact|{$~\not\vdash \RktMeta{e'} : \tau'$}|
     @exact|{\\}|
     then @exact|{$\untyped{\RktMeta{e}} \Downarrow \RktMeta{wrong}$}| or
     @exact|{$\untyped{\RktMeta{e}}$}| diverges.
  }}
]

If neither @exact|{\RktMeta{e}}| nor @exact|{\RktMeta{e'}}| type checks, then we have no guarantees
 about the run-time behavior of either term.
In a perfect world both would diverge, but the fundamental limitations of
 static typing@~cite[fagan-dissertation-1992] and computability
 keep us imperfect.

At present, these correctness requirements must be checked manually by the
 author of a function in @exact{$\interp$} or @exact{$\elab$}.


@; =============================================================================
@section{Cooperative Elaboration}

Suppose we implement a currying operation
 @exact{$\elabf$} such that e.g.
 @exact{$\llbracket$}@racket[(curry (λ (x y) x))]@exact{$\rrbracket~=~$}@racket[(curry_2 (λ (x y) x))].
The arity of @racket[(λ (x y) x)] is clear from its representation.
The arity of the result could also be derived from its textual representation,
 but it is simpler to add a @emph{tag} such that future elaborations
 can retrieve the arity of @racket[(curry_2 (λ (x y) x))].

Our implementation uses a tagging protocol, and this lets us share information
 between unrelated elaboration function in a bottom-up recursive style.
Formally speaking, this changes either the codomain of functions in @exact{$\elab$}
 or introduces an elaboration environment mapping expressions to values.

