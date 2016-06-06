#lang scribble/sigplan @onecolumn

@require["common.rkt"]

@title[#:tag "sec:examples"]{Small Things and Great}
@; He that lets
@; the small things bind him
@; leaves the great
@; undone behind him.

@Figure-ref{fig:expr} defines an untyped, call-by-value @racket[λ] calculus
 augmented with boolean, character, natural number, and vector literals.
The language also includes a set @exact{$\primop$} of primitive operations,
 described in @Figure-ref{fig:primop} and a labeled error term @exact{$\bot$}.
For the moment we focus on two primitive operations: @racket[ref], for dereferencing
 a vector, and @racket[map], for mapping a function of arity @exact{$m$} over
 the columns of an @exact{$m \times n$} matrix.
We specify these operations with an informal mathematical syntax using
 @racket[∈] and @racket[=] to dynamically test and pattern match value forms.

  @figure["fig:expr" "Term language"
    @exact|{\input{fig-language}}|

  ]

  @figure["fig:primop" "Primitive operations"
    @exact|{\input{fig-primops}}|
  ]

This language is intended to model the untyped core of realistic
 languages like ML, Haskell, and Typed Racket; albiet using @exact{$\lambda$}
 notation instead of a machine language.
Likewise, the polymorphic type system in @Figure-ref{fig:types} is a standard,
 implicitly quantified system.
@; These types are CLEARLY LIMITED in what they can express,
@; but even TR, ML+1stclassmodules, GHC+extensions have limitations
@; tho less obvious

  @figure["fig:types" "Type System"
    @exact|{\input{fig-types}}|
  ]

  @figure["fig:typed-primops" "Typed Primitive Operations"
    @exact|{\input{fig-typed-primops}}|
  ]

Using this type system, we can approximate the behavior of @racket[ref] and
 @racket[map] with the signatures in @Figure-ref{fig:typed-primops}.
The type @exact|{$\tau_{\RktMeta{ref}}$}| captures the essence of @racket[ref],
 but does not express the requirement that @emph{value} of the number
 @racket[i] in a call @exact|{\RktMeta{(ref~v~i)}}|
 must be strictly less than the length of the vector @racket[v].
The type @exact|{$\tau_{\RktMeta{map}}$}|, on the other hand,
 is limited compared to the untyped @racket[map] because there is no way
 to apply a typed function to an unknown number of arguments.@note{Curried functions
  run into the same limitation.}
  @;Try implementing @racket[apply f (x1 \ldots xn) = f x1 \ldots xn}.
Realistic languages would account for at least the 2-arity case with a second
 function @exact|{$\RktMeta{map}_2$}| and ask programmers
 to choose between @racket[map] and @exact|{$\RktMeta{map}_2$}| at each call site.
This inability of the type system to express our specifications is a problem.


@section{Refining the Imprecise Types}

There is no reason we could not revise our type system to make
 it possible to track the length of vectors and express polymorphism over
 the arity of functions.
In fact, GHC can express the type of fixed-length vectors@~cite[lb-sigplan-2014] and Typed Racket
 supports variable-arity polymorphism@~cite[stf-esop-2009].
But such revisions are sweeping changes to the type system and therefore the
 language.
Moreover, programmers who use the language to create embedded DSLs may
 wish to encode novel type constraints, as we demonstrate in @Secref{sec:regexp}.

Rather than alter the type system or accept the status quo,
 we add the syntax extension system shown in @Figure-ref{fig:syntax-base}.
To underscore the fact that syntax extensions are run on syntactic terms
 rather than runtime values, we format the system as a collection of
 inference rules.
In practice, we could implement these rules with a pattern matcher like
 Racket's @racket[syntax-parse].
@; Users extend by:
@; - inhertance, modifying definition of a "parser" object
@; - use parameter for recursive call, users update parameter

Define @exact{$\elaborate(e) = e'$} if and only if @exact{$e \expandsto e'$}.
As presented in @Figure-ref{fig:syntax-base}, @exact{$\elaborate$} is a structurally
 recursive identity function.
Still, it satisfies three key properties (the 3 R's of reasonable syntax extensions).
Let @exact{$\Downarrow$}
 implicitly quantify over closing substitutions @exact{$\gamma$}.

  @figure["fig:syntax-base" "Default Syntax Extensions"
    @exact|{\input{fig-syntax-base.tex}}|
  ]

  @theorem["Refinement"]{
    If @exact{$\vdash e : \tau$} then @exact{$\vdash \elaborate(e) : \tau'$}
     and @exact{$\tau' <: \tau$}.
    Furthermore @exact{$e \Downarrow v$} if and only if @exact{$\elaborate(e) \Downarrow v$}.
  }

  In other words, elaboration may make the type of a well-typed term more precise.
  (Use type equality in place of @exact{$<:$} for now)


  @theorem["Retraction"]{
    If @exact{$\not \exists \tau~.~\vdash e : \tau$} but @exact{$\vdash \elaborate(e) : \tau'$}
     then @exact{$e \Downarrow v$} if and only if @exact{$\elaborate(e) \Downarrow v$}.
  }

  Desugaring a term to make it type check does not change its dynamic behavior.
  (Holds trivially because its premise is never met.)


  @theorem["Relevance"]{
    If @exact|{$\elaborate(e) = \bot^{e'}$}| then @exact{$e'$} is a subterm of @exact{$e$}
     and evaluating @exact{$e'$} (but not necessarily @exact{$e$}) raises a runtime error.
  }

  Elaboration errors always reflect defects in the user's code and are presented
   using source terms rather than expanded terms.
  (Holds because the current version of @exact{$\elaborate$} never yields an error term.)

We claim that these theorems enable reasoning about un-elaborated terms
 using the familiar type system and semantics from @Figure-ref["fig:expr" "fig:types"].
Henceforth, we say that a term @racket[e] typechecks in the extended language
 (written @exact{$\vdashe e : \tau$})
 if there exists a type @exact{$\tau$} such that @exact|{$\Gamma_{\primop} \vdash \elaborate(e) : \tau$}|.
A term @racket[e] evaluates to a value @racket[v] in the extended language
 (@exact{$e \Downarrowe v$}) if and only if @exact{$\elaborate(e) \Downarrow v$}.

The crucial feature of @exact{$\elaborate$} is that language users can add
 cases to it.
Changing roles from language designers to language users, we leverage this
 ability in the next sections based on the intuition that the syntax
 of value forms carries information.


@section{Bounds Checking for @racket[ref]}

For a first extension, we can check vector references at compile-time when
 @racket[ref] is called with a vector literal and a natural number.
To accomodate future syntax extensions, we recursively expand the arguments to
 our @racket[ref] extension before checking.

  @exact|{
    \begin{mathpar}
      \inferrule{
        v \expandsto \vectoren
        \\
        i \expandsto i'
        \\\\
        i' \in \naturals
        \\
        i' < n
      }{
        \RktMeta{ref}~v~i \expandsto \RktMeta{ref}~\vectoren~i
      }
    \end{mathpar}
  }|

When this condition is not met, 


@section{Typed, Generalized @racket[map]}



@section[#:tag "sec:regexp"]{Adding Regular Expressions}

@; - string-embedded DSL
@; - NOT part of core language, just ideas from crazy library writer
@; - refine codomain of an operation

We assume @exact{$\Sigma$} is just the lowercase characters @exact{$a \ldots z$}.
To keep the language small, we define strings as vectors of characters
 and use e.g. @racket{abc} as shorthand for @exact|{$\vectorgen{a, b, c}$}|.


@section[#:tag "sec:define"]{Handling Variables}
@; Add a type system --- thanks Alex!
