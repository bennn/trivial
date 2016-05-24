#lang scribble/sigplan @onecolumn


@require["common.rkt" (only-in scribble/base nested)]


@title[#:tag "sec:intro"]{Introduction}

Typecheckers for polymorphic languages like Haskell incredibly wasteful.
Given an AST, they immediately throw away the rich @emph{syntax} of expression
 and value forms in favor of a @emph{type} abstraction.
So whereas any novice programmer can tell that the expression @tt{(/ 1 0)}
 will go wrong, the typechecker cannot because all it sees is @tt{(/ @emph{Num} @emph{Num})}.

These typecheckers are also incredibly stubborn, and cannot handle basic
 forms of polymorphism that are not part of their type algebra.
For instance, the behavior of the function @tt{map} can be stated in clear
 English:

 @nested[@elem[#:style 'italic]{
   @tt{(map proc lst ...+) → list?}
   Applies @tt{proc} to the elements of the @tt{lsts} from the first elements to the last.
   The @tt{proc} argument must accept the same number of arguments as the number
   of supplied @tt{lsts}, and all @tt{lsts} must have the same number of elements.
   The result is a list containing each result of @tt{proc} in order.
 }]

But the Haskell typechecker can only validate indexed versions of @tt{map}
 defined for a fixed number of lists.
Programmers are thus forced to choose between @tt{map}, @tt{zipWith}, and @tt{zipWith3}
 depending on the number of lists they want to map over.@note{Maybe need a better
  example than @tt{map} because polydots handle it. @tt{curry}?}

There are a few solutions to these problems. Blah blah.
They all require migrating to a new programming language or instrumenting call
 sites across the program, neither of which are desirable solutions for the
 working programmer.

We propose @emph{extending} a language's type system with
 @emph{introspective} typing rules that capture value information as well as type
 information.
Given a proof theory @exact|{$\Progn \vDash {\tt P}$}| for inferring propositions
 @tt{P} from the program text @exact{$\Progn$}, the rule for division would be:

  @exact|{
    \begin{mathpar}
      \inferrule*[]{
          \Gamma; \Progn \vdash n_1 : \tnum
          \\
          \Gamma; \Progn \vdash n_2 : \tnum
          \\\\
          \Progn \vDash n_2 \neq 0
        }{
          \Gamma; \Progn \vdash {\tt /}~n_1~n_2 : \tnum
      }
    \end{mathpar}
  }|

and the rule for @tt{map} would be:

  @exact|{
    \begin{mathpar}
      \inferrule*{
          \Gamma; \Progn \vdash f : \overline{\tvar{A}} \rightarrow \tvar{B}
          \\
          \Gamma; \Progn \vdash \overline{x} : \overline{\tlist{A}}
          \\\\
          \Progn \vDash {\tt length}(\overline{\tvar{A}}) = {\tt length}(\overline{\tlist{A}})
        }{
          \Gamma; \Progn \vdash {\tt map}~f~\overline{x} : \tlist{B}
      }
    \end{mathpar}
  }|

These rules would augment---not replace---the existing rules for division
 and @tt{map}.
In the case that no information about subterms can be inferred from the program
 text, we defer to the division rule that admits runtime errors and the @tt{map}
 rule that can only be called with a single list.


@; =============================================================================
@parag{Contributions}
@itemlist[
  @item{
    We propose ``typechecking modulo theories'' (or something):
     using the text of the program to refine typechecking judgments.
  }
  @item{
    As a proof of concept, we implement a simple theory within the Racket
     macro expander and use it enhance the user experience of Typed Racket.
    The implementation is just a library; we suspect Typed Clojure and TypeScript
     programmers could build similar libraries using Clojure's macros and sweet.js.
  }
  @item{
    Our Racket implementation leverages a novel class of macros.
    We define the class, state proof conditions for a new macro
     to enter the class, and prove our macros are in the class.
  }
]


@; =============================================================================
@parag{Guarantees}
@itemlist[
  @item{
    Refinement of the existing type system.
    Adding @exact{$\Progn \vDash$} only rejects programs that would go
     wrong dynamically and/or accept programs that are ill-typed, but go
     right dynamically.
  }
  @item{
    Errors reported by our implementation are in terms of the source program,
     not of expanded/transformed code.
  }
]


@; =============================================================================
@parag{Applications}

This table summarizes the ICFP pearl.
We infer values from various syntactic domains and use the inferred data to
 justify small program transformations.

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
         #:column-properties '(right right right)
  (list (list @bold{Domain}      @bold{Infers}    @bold{Use})
        (list "format strings"   "arity + type"   "guard input")
        (list "regexp strings"   "# groups"       "prove output")
        (list "λ"                "arity"          "implement curry_i")
        (list "numbers"          "value"          "constant folding")
        (list "vectors"          "size"           "guard, optimize")
        (list "sql schema"       "types"          "guard input, prove output"))]

@; =============================================================================
@parag{Lineage}

Herman and Meunier used macros for partial evaluation of string DSLs@~cite[hm-icfp-2004].
Lorenzen and Erdweg defined criteria for sane deguarings@~cite[le-popl-2016].

