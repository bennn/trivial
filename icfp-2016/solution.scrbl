#lang scribble/sigplan @onecolumn
@; TODO color p?, e
@; Better notation for erasure, maybe just color differently

@require["common.rkt"]

@title[#:tag "sec:solution"]{Interpretations, Elaborations}

A textualist elaborator (henceforth, @emph{elaborator})
 is a specific kind of macro, meant to be run on the syntax of a program
 before the program is type-checked.
The behavior of an elaborator is split between two functions: interpretation
 and elaboration.

An @emph{interpretation} function attempts to parse data from an expression.
In the Lisp tradition, we will use the value @racket[#false] to indicate
 failure and refer to interpretation functions as @emph{predicates}.
Using @exact|{\RktMeta{expr}}| to denote the set of syntactically valid, symbolic
 program expressions
 and @exact|{\RktVal{val}}| to denote the set of symbolic values,
 we define the set @exact{$\interp$}
 of interpretation functions.

  @exact|{$$\interp\ : \big\{\RktMeta{expr} \rightarrow ({\RktVal{val}} \cup {\tt \RktVal{\#false}})\big\}$$}|

If @exact|{${\tt p?} \in \interp$}| and @exact|{${\tt e} \in \emph{expr}$}|,
 it may be useful to think of
 @exact|{${\tt (p?~e)}$}| as @emph{evidence} that the expression @exact|{${\tt e}$}|
 is recognized by @exact|{${\tt p?}$}|.
Alternatively, @exact|{${\tt (p?~e)}$}| is a kind of interpolant@~cite[c-jsl-1997],
 representing key data embedded in @exact|{${\tt e}$}|.
Correct interpretation functions @exact|{${\tt p?}$}| obey three guidelines:

@itemize[
  @item{
    The expressions for which @exact|{${\tt p?}$}| returns a non-@racket[#false]
     value must have some common structure.
  }
  @item{
    Non-@racket[#false] results @exact|{${\tt (p?~e)}$}| are computed by a
     uniform algorithm and must have some common structure.
  }
]

This vague notion of common structure may be expressible as a type in an
 appropriate type system.
It is definitely not a type in the target language's type system.

Functions in the set @exact|{$\elab$}| of @emph{elaborations}
 map expressions to expressions.
We write elaboration functions as @exact{$\elabf$} and their application
 to an expression @exact{$e$} as @exact{$\llbracket e \rrbracket$}.
Elaborations are allowed to fail raising syntax errors, which we notate as
 @exact|{$\bot$}|.

    @exact|{$$\elab : \big\{ {\RktMeta{expr}} \rightarrow ({\RktMeta{expr}} \cup \bot)\big\} $$}|

The correctness specification for an elaborator @exact{$\elabf \in \elab$}
 is defined in terms of the language's typing judgment @exact|{$~\vdash {\tt e} : \tau$}|
 and evaluation relation @exact|{$\untyped{{\tt e}} \Downarrow {\tt v}$}|.
The notation @exact|{$\untyped{{\tt e}}$}| is the untyped erasure of @exact|{${\tt e}$}|.
We also assume a subtyping relation @exact|{$\subt$}| on types.
Let @exact|{$\elabfe{{\tt e}} = {\tt e'}$}|:

@itemlist[
  @item{@emph{
    If @exact|{$~\vdash {\tt e} : \tau$}| and @exact|{$~\vdash {\tt e'} : \tau'$}|
    @exact|{\\}|
    then @exact|{$\tau' \subt \tau$}|
    @exact|{\\}|
    and both
     @exact|{$\untyped{{\tt e}} \Downarrow {\tt v}$}| and
     @exact|{$\untyped{{\tt e'}} \Downarrow {\tt v}$}|.
  }}
  @; e:t e':t' => t' <: t /\ e <=> e'

  @item{@emph{
    If @exact|{$~\not\vdash {\tt e} : \tau$}| but @exact|{$~\vdash {\tt e'} : \tau'$}|
     @exact|{\\}|
     then @exact|{$\untyped{{\tt e}} \Downarrow {\tt v}$}| and
     @exact|{$\untyped{{\tt e'}} \Downarrow {\tt v}$}|.
  }}
  @; -e:t e':t' => e <=> e'

  @item{@emph{
    If @exact|{$~\vdash {\tt e} : \tau$}| but @exact|{${\tt e'} = \bot$}|
     or @exact|{$~\not\vdash {\tt e'} : \tau'$}|
     @exact|{\\}|
     then @exact|{$\untyped{{\tt e}} \Downarrow \mathsf{wrong}$}| or
     @exact|{$\untyped{{\tt e}}$}| diverges.
  }}
  @; e:t -e':t' => e^
]

If neither @exact|{${\tt e}$}| nor @exact|{${\tt e'}$}| type checks, then we have no guarantees
 about the run-time behavior of either term.
In a perfect world both would diverge, but the fundamental limitations of
 static typing@~cite[fagan-dissertation-1992] and computability
 keep us imperfect.

At present, these correctness requirements must be checked manually by the
 author of a function in @exact{$\interp$} or @exact{$\elab$}.

