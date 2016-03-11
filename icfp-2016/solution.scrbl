#lang scribble/sigplan @onecolumn


@require["common.rkt"]

@title[#:tag "sec:solution"]{Interpretations and Translations}

The out-of-bounds reference in @code{[0,1,2] !! 3} is evident from the
 definition of @code{!!} and the values passed to it.
We also know that @code{1 `div` 0} will go wrong because division by zero is
 mathematically undefined.
Similar reasoning based on the meaning of @racket{%d} and the number of variables
 bound by anonymous functions like @code{\ x y z -> x} can determine the correctness
 of calls to @racket[printf] and @racket[curry].

In general, our analysis begins with a class of predicates for extracting
 meta-information from expressions; for example the length of a
 list value or arity of a procedure.

    @exact|{$$\interp\ : \big\{\emph{expr} \rightarrow \emph{Maybe(value)}\big\}$$}|

Applying a function @code{f} @exact|{$\in \interp\ $}| to a syntactically
 well-formed expression should either yield a value describing some aspect
 of the input expression or return a failure result.@note{The name @emph{interp}
  is a mnemonic for @emph{interpret} or @emph{interpolant}@~cite[c-jsl-1997].}
Correct predicates @code{f} should recognize expressions with some common
 structure (not necessarily the expressions' type) and apply a uniform algorithm
 to computer their result.
The reason for specifying @exact|{$\interp\ $}| over expressions rather than
 values will be made clear in @Secref{sec:usage}.

Once we have predicates for extracting data from the syntax of expressions,
 we can use the data to guide program transformations.
The main result of this pearl is defining a compelling set of such transformations.

    @exact|{$$\trans : \big\{ \emph{expr} \rightarrow \emph{expr}\big\} $$}|

Each @exact|{${\tt g} \in \trans$}| is a partial function such that @exact|{${\tt (g~e)}$}|
 returns either a specialized expression @exact|{${\tt e'}$}| or fails due to a value
 error.
These transformations should be applied to expressions @exact|{${\tt e}$}| before
 type-checking; the critera for correct transformations can then be given
 in terms of the language's typing judgment @exact|{$\vdash {\tt e} : \tau$}|
 and untyped evaluation relation @exact|{$\untyped{{\tt e}} \Downarrow {\tt v}$}|,
 where @exact|{$\untyped{{\tt e}}$}| is the untyped erasure of @exact|{${\tt e}$}|.
We also assume a subtyping relation @exact|{$\subt$}| on types.

@itemlist[
  @item{@emph{
    If @exact|{$\vdash {\tt e} : \tau$}| and @exact|{$\vdash {\tt e'} : \tau'$}|
     then @exact|{$\tau' \subt \tau$}| and both
     @exact|{$\untyped{{\tt e}} \Downarrow {\tt v}$}| and
     @exact|{$\untyped{{\tt e'}} \Downarrow {\tt v}$}|.
  }}
  @; e:t e':t' => t' <: t /\ e <=> e'

  @item{@emph{
    If @exact|{$\not\vdash {\tt e} : \tau$}| but @exact|{$\vdash {\tt e'} : \tau'$}|
     then @exact|{$\untyped{{\tt e}} \Downarrow {\tt v}$}| and
     @exact|{$\untyped{{\tt e'}} \Downarrow {\tt v}$}|.
  }}
  @; -e:t e':t' => e <=> e'

  @item{@emph{
    If @exact|{$\vdash {\tt e} : \tau$}| but @exact|{${\tt e'} = \bot$}|
     or @exact|{$\not\vdash {\tt e'} : \tau'$}|
     then @exact|{$\untyped{{\tt e}} \Downarrow \mathsf{wrong}$}| or diverges.
  }}
  @; e:t -e':t' => e^
]

If neither @exact|{${\tt e}$}| nor @exact|{${\tt e'}$}| type checks, then we have no guarantees
 about the run-time behavior of either term.
The hope is that both diverge, but proving this fact in a realistic language is
 more trouble than it is worth.

@; Erasure, moral, immoral
Finally, we say that a translation @exact|{${\tt (g~e) = e'}$}| is @emph{moral} if
 @exact|{$\untyped{{\tt e}}$}| is @exact|{$\alpha$}|-equivalent to @exact|{$\untyped{{\tt e'}}$}|.
Otherwise, the tranlation has altered more than just type annotations and is
 @emph{immoral}.
All our examples in @Secref{sec:intro} can be implemented as moral translations.
Immoral translations are harder to show correct, but also much more useful.


@; @section{Model}
@; Traditional descriptions of typed calculi follow a two-stage approach.
@; Expressions are first type-checked, then evaluated.
@; For our framework we need to introduce a first step, elaboration,
@;  which takes place before type checking.
@; 
@; To this end, we adapt the macro system model from Flatt@~cite[f-popl-2016]
@;  with a simple type system.
@; Syntactically-valid expressions in the language are initially untyped.
@; The first set of 



