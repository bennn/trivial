#lang scribble/sigplan @onecolumn

@; Goal of Sec 2 is to introduce background on TR needed to:
@; - understand our bias
@; - understand our examples
@; - understand "type elaborator api"


@require["common.rkt"]

@title[#:tag "sec:background"]{Towards a Type Elaborator API}
@; A poet should be of the
@; old-fahioned meaningless brand:
@; obscure, esoteric, symbolic,
@; -- the critics demand it;
@; so if there's a poem of mine
@; that you do understand
@; I'll gladly explain what it means
@; till you don't understand it.

@; TODO
@; - can we not use "Typed Racket" as the first word of this section?
@;   to me that's a huge turn-off after reading the introduction.
@; - missing answer: why are we using TR

Typed Racket does not have a type elaboration API; however, it inherits a rich
 syntax extension system from its host language, Racket.
Experience with Racket syntax extensions (aka macros) motives our proposal for
 a similar type elaboration system, presented in @Secref{subsec:api}.
Moreover, we have implemented the transformations described in @Secref{sec:define}
 as a Typed Racket package
 and this section is intended to prepare the way for later code snippets.


@section{Typed Racket, today}

Typed Racket is an ongoing experiment in language design the success of which
 is a testament to the usefulness and versatility of syntax extensions.
The entire language is implemented as a Racket library; in particular, a
 library of syntax extensions.
Types in Typed Racket are distinguished Racket terms, given special meaning by
 the type checker.
The checker itself is nothing more than a Racket function defined over
 type-annotated programs and run before the program is compiled.
When typechecking succeeds, the annotations are erased and the resulting
 program is fed to the Racket compiler.
As such, there is no need for a dedicated ``Typed Racket compiler''.
Type-driven optimizations occur on core Racket forms just after type checking
 and the result feeds in to the existing compiler toolchain.

@; One might expect this is kinda slow. Maybe that's true. But has benefits and users.

For a concrete example, have a left-leaning factorial function:

  @codeblock{
    #lang typed/racket
    (define (fact (n : Natural)) : Natural
      (foldl * n (range 1 n)))
  }

Aside from the type annotations, the function is ordinary Racket code
 and behaves as such when run.
But the syntax accepted by Typed Racket's @racket[define] is a superset of
 valid, untyped Racket.
Furthermore, this @racket[define] has an extended semantics.
When the above program is compiled, @racket[define] registers the identifier
 @racket[fact] in a type environment with the signature @racket[(Natural -> Natural)].
The definition then expands to an annotation-free Racket @racket[define] and
 the same expansion-and-type-collection process is repeated on the body of @racket[fact].
So just as Typed Racket re-uses the Racket compiler, Typed Racket's @racket[define]
 re-uses the semantics of Racket's.
This sleight of hand is accomplished by shadowing @racket[define] with a syntax
 extension that moves types from the program syntax to a type environment---and
 crucially, does nothing more.

Note, however, that no types are checked at this point.
It is only after the entire program is expanded to core Racket and all type
 definitions collected that types are checked and e.g. the type variables
 for @racket[foldl] are instantiated.
Waiting until all syntax extensions are expanded is both a pragmatic choice
 and allows extensions to create and refine types written in a program without
 subverting the authoritarian type checker.

@; not really liking 'pragmatic' but I guess it should be obvious, that's the
@; easiest way to implement a TC for Racket


@section{Racket Macros, quickly}

Having built some intuition for how Typed Racket's @racket[define] operates,
 we use its definition to introduce Racket macros.@note{After this section,
    we will stop using the term @emph{macro} in favor of the more general
    phrase @emph{syntax extension}.}
The following is paraphrased from Typed Racket and elaborates a type-annotated
 @racket[define] to an unannotated one.

  @racketblock[
    (define-syntax (-define stx)
      (syntax-parse stx #:literals (:)
        [(-define (nm:id (arg:id : ty)) : ret-ty body ...)
         (define type #`(ty -> ret-ty))
         (register-type #`nm type)
         #`(define (nm arg) body ...)]))
  ]

Going line-by-line, we have:

@; TODO what is the point of each?
@; TODO what is the bottom line?

@itemlist[
  @item{
    @racket[define-syntax] declares a function on code; in other words,
     a macro.
    The formal parameter @racket[stx] is so named because it always binds
     a @emph{syntax object} representing the context of a call to the
     @racket[-define] macro.
  }
  @item{
    @racket[syntax-parse] is pattern matching for syntax objects.
    The optional argument @racket[#:literals (:)] causes any @racket[:] characters
     in a @racket[syntax-parse] pattern to match only against the @racket[:]
     identifier bound in the current lexical scope.
    Normally the variable @racket[:] is no different from the pattern @racket[x]
     or the pattern @racket[any-variable].
  }
  @item{
    The third line of the macro is a pattern.
    The remaining lines are instructions to perform if the pattern matches.
    Within the pattern:
    @itemlist[
      @item{
        @racket[-define], @racket[nm], @racket[arg], @racket[ty],
         @racket[ret-ty], and @racket[body] are @emph{pattern variables} matched to
         sub-expressions of @racket[stx].
      }
      @item{
        The ellipses (@racket[...]) are part of the grammar of @racket[syntax-parse]
         and match zero or more occurrences of the previous pattern,
         to the effect that @racket[body ...] matches a list of consecutive expressions.
      }
      @item{
        @racket[:id], as in @racket[nm:id] and @racket[arg:id], is a
         @emph{syntax class} annotation.
        Using the @racket[id] syntax class causes @racket[nm] and @racket[arg]
         to only match identifiers and not, for instance, integer constants or
         parenthesized expressions.
      }
    ]
  }
  @item{
    @|stx| creates a syntax object from an expression.
    In this case, the syntax object is a function type, built from our pattern
     variables and Typed Racket's @racket[->] constructor.
  }
  @item{
    @racket[register-type] binds an identifier to a type in a global type environment.
    Since the pattern variable @racket[nm] is only bound to a symbol, we use
     the syntax constructor @|stx| to associate the symbol with
     the lexical scope of @racket[stx].
  }
  @item{
    The result of any @racket[syntax-parse] pattern must be a syntax object.
    Here we build a Racket @racket[define] statement from the relevant pattern
     variables.
    On this line, the ellipses (@racket[...]) are used in a dual sense to
     splice the contents of the list @racket[body] into the body of the new
     @racket[define] statement.
  }
]

A call to @racket[syntax-parse] can contain multiple patterns.
Indeed,
 the actual definition of Typed Racket's define has patterns for
 non-function definitions @racket[(define n : Integer 42)]
 and unannotated @racket[define] statements.
Finally, the module that implements @racket[-define] exports it as @racket[define]
 to change the behavior of definitions only in external modules.

@; Include a jab about parenthesized syntax making metaprogramming life easier?
@; @subsection{Oh, the Parentheses}


@section[#:tag "subsec:api"]{Implementing a Type Elaborator}

Just as Typed Racket parses the syntax of a program and extracts breadcrumbs
 for the type environment, a type elaborator transforms
 a program leaving hints to guide the type checker.
Syntax extensions are a low-level way to achieve this behavior.

@Figure-ref{fig:printf} demonstrates a type-elaborated variant of Racket's
 @racket[printf].
When called with a string literal @racket[fmt] as its first argument, the
 elaborator @racket[-printf]
 reads @racket[fmt] for type and arity constraints using the function @racket[format-types].
For instance, the format string @racket["~b"] would produce the type constraint
 @racket[(Exact-Number)], implying that the arguments @racket[args] must be a
 list with one element of type @racket[Exact-Number].
This constraint is used twice in @racket[-printf]:
 first to check the length of @racket[args] against the length of @racket[types]
 and second to add explicit type annotations (via @racket[ann]) around each
 argument to the format string.
Whereas Typed Racket accepts any number of values with any type and
 lets Racket's @racket[printf] raise runtime errors, type elaboration reports
 both arity and type errors statically.
@; arity error = caught directly
@; type error = implied

  @figure["fig:printf" "Type elaboration for format strings"
    @racketblock[
      (define-syntax (-printf stx)
        (syntax-parse stx
         [(-printf fmt:str args ...)
          #:with (types ...) (format-types #`fmt)
          (if (= (stx-length #`(args ...))
                 (stx-length #`(types ...)))
            #`(printf fmt (ann args types) ...)
            (error 'printf "arity mismatch in ~s" stx))]))
    ]
    @; Include the default branch? I just don't know what to say about it.
    @;     [(-printf fmt args ...)
    @;      #`(printf fmt args ...)]))
  ]

In general, the high-level goals of such type elaborations are:
  @itemlist[
    @item{ (@goal{refinement})
      Refine type signatures using latent, syntactic @emph{value information},
       such as the characters in a string constant.
    }
    @item{ (@goal{reuse})
      Rely on the existing type checker.
      Avoid duplicating its work and never resort to proofs by assertion.@note[@elem{
        Inspired by the @emph{translation validation} approach to
         compiler correctness @~cite[pss-tacas-1998].}]
      @; Just trying to say, always typecheck things
    }
    @item{ (@goal{relevance})
      Report errors in terms of the programmer's code, not in terms
       of elaborated code.@note[@elem{Inspired by SoundX @~cite[le-popl-2016].}]
    }
  ]

The @racket[-printf] elaborator meets these goals by producing Typed Racket code
 that only adds type annotations to the original program.
If these annotations fail, they report a type error relative to an element
 of @racket[args].
As for refining the types, @racket[-printf] is best described with a quasi-dependent
 type in terms of a proof theory @exact{$\Sigma$}.

 @exact|{\begin{mathpar}
   \inferrule{
     \Sigma \vdash (\RktMeta{format-types}~\RktMeta{fmt}) = \tau_0 \ldots \tau_{n-1}
     \\\\
     \typestogen{\penv;\,\tenv}{\RktMeta{arg}_0}{\tau_0}
     \\
     \ldots
     \\
     \typestogen{\penv;\,\tenv}{\RktMeta{arg}_{n-1}}{\tau_{n-1}}
   }{
     \typestogen{\penv;\,\tenv}{\RktMeta{-printf fmt}~\RktMeta{arg}_0 \ldots \RktMeta{arg}_{n-1}}{\mathsf{Unit}}
   }
 \end{mathpar}}|

We chose @racket[printf] as an introductory example because its correctness
 and type soundness follow directly from the soundness of @racket[format-types].
Correctness is not generally so simple to "prove", so @Secref{sec:segfault} and @Secref{sec:regexp}
 show how type elaboration can justify a potentially unsafe optimizing transformation
 and give library authors a technique for implementing a precise API without
 changing their language's type system.

