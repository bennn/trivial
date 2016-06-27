#lang scribble/sigplan @onecolumn
@; 1. MODEL
@; 2. IMPLEMENTATION
@; 3. EVALUATION

@; Q. remove <e ... e>, and only have vector values?
@;    because the syntax extension rules expect only values (expressions ruin the proofs)

@require[
  "common.rkt"
  "evaluation.rkt"
  glob
  racket/sequence
  (only-in racket/list take)
  (only-in trivial/private/raco-command collect-and-summarize)
]

@title[#:tag "sec:segfault"]{When using the API can cause a segfault}

Every programming languages comes with arrays. In Typed Racket, array
facilities come as a library that essentially exports constructors,
dereferencing functions, and mutation operations. SML similarly provides
them from a run-time library@~cite[gr-cup-2004].
The API for array libraries tends to come with highly conservative
signatures. For example, an array indexing operation calls for an array
and an integer and then produces the designated element from the given
array; run-time checks ensure that the integer is in the interval
@math{[0,n)} where @math{n} is the length of the array. 

To speed up program execution, Typed Racket has access to an unsafe array
indexing operation. Like array indexing in C, this operation
retrieves the bits at the specified location without checking the size of
the index. If used inappropriately, such an unsafe operation can cause the
program to print random results or to segfault. 

In this section we show that the creator of the Typed Racket array library
can replace checked array indexing with unsafe indexing. While this use of
type tailoring is quite simple, it supplies a great case study. To begin
with, it requires an innovation on the standard progress and preservation
method for showing type soundness. Specifically, the author of the array
library must show that the evaluator remains a function and does not
introduce segfaults into typed programs without run-time checks for
indexing (@secref{sec:segfault:model}). The Typed Racket implementation is quite
straightforward; the core consists of two dozen lines
(@secref{sec:segfault:implementation}). Finally, an evaluation on the Racket code
base indicates that the prototype is highly effective for a certain style of
programming.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:segfault:model"]{Elaborating array indexing}

@Figure-ref{fig:stlc} describes a simply typed @exact{$\lambda$} calculus
 with integers and integer arrays.
@Figure-ref{fig:elab0} gives a type-directed elaboration of terms written
 in this surface language to a typed, executable core language.
The main difference between surface and core is that array references
 @exact{$\aref{e}{e}$} are replaced with calls to a primitive operation
 @exact{$\checkedref$}.
For example, the term @exact|{$\aref{x}{3}$}| elaborates to @exact|{$\checkedref~x~3$}|
 when @exact{$x$} is a variable with type @exact{$\tarray$}.

The operational semantics for the core language are given in @Figure-ref{fig:stlc-core},
 along with type judgments for two primitive operations:
 @exact{$\checkedref$} and @exact{$\unsaferef$}.
Intuitively, @exact{$\checkedref$} is a memory-safe function that performs a bounds check before
 dereferencing an array and raises a runtime error when called with incompatible values.
On the other hand, @exact{$\unsaferef$} does not perform a bounds check and therefore
 may return arbitrary data or raise a memory error when called with an invalid index.
These negative consequences are modeled in the (family of) rule(s) @sc{E-UnsafeFail}.
Consequently, the judgment @exact{$\smallstep{e}{e}$} and its transitive closure
 @exact{$\smallstepstar{e}{e}$} are relations, whereas
 typing and elaboration are proper functions.

 @figure["fig:stlc" "Simply-Typed λ Calculus" #:style left-figure-style
  @exact|{\input{fig-stlc-surface}}|
 ]

 @figure["fig:elab0" "Elaborating typed terms" #:style left-figure-style
   @exact|{\input{fig-elab0}}|
 ]

 @figure["fig:stlc-core" "Core Language" #:style left-figure-style
   @exact|{\input{fig-stlc-core}}|
 ]

Evaluation of the surface language in @Figure-ref{fig:stlc}, however,
 is deterministic.

  @definition["evaluation"]{
    @exact{$\bigstep{e}{v}$} if and only if @exact{$e$} is closed and there
     exists @exact{$e'$}, @exact{$\tau$} such that
     @exact|{$\elabstoclosed{e}{e'}{\tau}$}|
     and @exact{$\smallstepstar{e'}{v}$}.
  }

  @theorem["determinism"]{
    Evaluation is a proper function.
    In other words, if @exact{$\bigstep{e}{v}$}
     and @exact{$\bigstep{e}{v'}$}
     then @exact{$v = v'$}.
  }

  @proof{
    @; (shorter, but repeats following paragraph)
    @; The surface language does not allow @exact{$\unsaferef$} and elaboration
    @;  does not introduce unsafe references, therefore the non-deterministic
    @;  rule @sc{E-UnsafeFail} is never used.

    By induction on terms of the core language, if
     @exact{$\smallstep{e}{e'}$} and @exact{$e$} is not of the form
     @exact{$\unsaferef~e_1~e_2$} then @exact{$e'$} is unique.
    The theorem follows because the surface language does not allow
     @exact{$\unsaferef$} and elaboration does not introduce unsafe references.
  }

Determinism implies that evaluation never invokes the @sc{E-UnsafeFail} rule;
 therefore, evaluation of surface terms is memory safe.
Additionally, the surface language is type safe.

  @theorem["soundness"]{
    If @exact{$e$} is closed and @exact|{$\elabstoclosed{e}{e'}{\tau}$}|
     then one of the following holds:
    @itemlist[
      @item{
        @exact{$e'$} is a value
      }
      @item{
        @exact{$\smallstep{e'}{\indexerror}$}
      }
      @item{
        @exact{$\smallstep{e'}{e''}$}
        and @exact{$\typestoclosed{e''}{\tau}$}
      }
    ]
  }

  @proof{
    The interesting cases are for array references, for which we observe that
     @exact|{$\elabstoclosed{\aref{e_1}{e_2}}{\checkedref~e_1~e_2}{\tint}$}|
     implies @exact{$\typestoclosed{e_1}{\tarray}$}
     and @exact{$\typestoclosed{e_2}{\tint}$}.
    Depending on the values of @exact{$e_1$} and @exact{$e_2$},
     the application steps to either @exact{$\indexerror$} or the @exact{$e_2$}-th
     element of the array @exact{$e_1$}---which, by assumption, has type @exact{$\tint$}.
  }


Building on the foundation of type soundness, we extend the elaborator with
 the @emph{value-directed} rules in @Figure-ref{fig:elab1}.
When the elaborator can prove that an array reference is in bounds based on
 the syntax of terms, it produces an unsafe reference.
Conversely, the elaborator raises a compile-time index error if it can prove
 the index is outside the bounds of the array.

  @figure["fig:elab1" "Value-directed elaboration" #:style left-figure-style
    @exact|{\input{fig-elab1}}|
  ]

Our thesis is that adding such rules does not undermine the safety guarantees of the
 original language.
In this case, we recover determinism and soundness by extending the proofs of
 type and memory safety to address the @sc{S-RefPass} and @sc{S-RefFail} elaboration rules.

  @definition[@exact{$\elabarrowplus$}]{
    The relation @exact{$\elabarrowplus$} is the union of the rules in
     @Figure-ref["fig:elab0" "fig:elab1"].
  }

  @theorem["determinism"]{
    @exact|{$\bigstepplus{e}{v}$}| is a proper function, where @exact|{$\bigstepplus{e}{v}$}|
     if and only if @exact|{$\elabstoclosedplus{e}{e'}{\tau}$}|
     and @exact|{$\smallstepstar{e'}{v}$}|.
  }

  @proof{
    With the original elaboration scheme, all array references @exact{$\aref{e_1}{e_2}$}
     elaborate via @sc{S-Ref} to @exact{$\checkedref~e_1'~e_2'$}, where @exact{$e_1'$}
     and @exact{$e_2'$} are the elaboration of @exact{$e_1$} and @exact{$e_2$}.
    References for which only @sc{S-Ref} applies remain determinsitic, but
     we have two cases where a new rule may also be used:
    @itemize[
      @item{ Case @sc{S-RefFail}:
        @exact{$e_1'$} is an array literal @exact{$\vectorvn$}
         and @exact{$e_2'$} is an integer literal @exact{$i \in \ints$}
         and either @exact{$i < 0$} or @exact{$i \ge n$}.
        If @sc{S-Ref} is chosen, @exact{$e'$} is @exact{$\checkedref~\vectorvn~i$}
         and evaluates to @exact{$\indexerror$} because @exact{$i$} is outside
         the bounds of the array.
        On the other hand, @sc{S-RefFail} raises the index error immediately.
      }
      @item{ Case @sc{S-RefPass}:
        @exact{$e_1'$} is an array literal @exact{$\vectorvn$}
         and @exact{$e_2'$} is an integer literal @exact{$i \in \ints$}
         and @exact{$0 \le i < n$}.
        If @sc{S-Ref} is chosen then @exact{$e'$} is @exact{$\checkedref~\vectorvn~i$}.
        If @sc{Ref-RefPass} is chosen then @exact{$e'$} is @exact{$\unsaferef~\vectorvn~i$}.
        Because @exact{$0 \le i < n$}, evaluation must proceed with
         @sc{E-CheckPass} or @sc{E-UnsafePass} respectively, producing
         @exact{$v_i$} in any event.
      }
    ]
  }

  For practical purposes, non-determinism in the elaborator should be
   resolved by giving the rules in @Figure-ref{fig:elab1} preference over the
   rules in @Figure-ref{fig:elab0}.
  But knowing that the result is the same for the non-identity elaborations
   gives us confidence in their correctness.

  @theorem["soundness"]{
    If @exact{$e$} is a closed term then @exact|{$\elabstoclosedplus{e}{e'}{\tau}$}|
     implies that @exact{$e'$} is either a value or steps to an index error or
     steps to a term @exact{$e''$} with type @exact{$\tau$}.
  }

  @proof{
    We extend the existing proof with cases for @exact{$\unsaferef$}.
    @itemize[
      @item{
        Case @sc{S-RefPass}: @exact{$e'$} is
         the call @exact{$\unsaferef~\vectorvn~i$} and @exact{$0 \le i < n$}.
        By assumption, @exact|{$\typestoclosed{\vectorvn}{\tarray}$}| and
         @exact{$\typestoclosed{i}{\tint}$}.
        Therefore @exact{$e'$} is well-typed by the rule @sc{T-Unsafe} and steps
         to the integer value @exact{$v_i$} by @sc{E-UnsafePass}.
      }
      @item{
        Case @sc{S-RefFail}: then @exact{$e'$} is
         @exact{$\indexerror$} and type soundness follows trivially.
      }
    ]
  }

The key step in both theorems is that elaboration supplies the proposition
 @exact{$0 \le i < n$} required for safe evaluation.
So long as assumptions like the above are properly stated by language implementors,
 type tailoring library authors can help to meet them.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:segfault:implementation"]{Implementing the elaborator}

@; WHY ... WHY ARE THEY ALWAYS PLAYING JAZZ IN THE LOBBY OF THIS HOTEL ???
@; (Asked the concierge: it's always on, on a loop. For Christmas it's Christmas jazz)

We have implemented the rules in @Figure-ref{fig:elab1} as a syntax extension for Typed Racket.
Including imports and exports, the implementation is an 18 line module (@Figure-ref{fig:vector-ref-extension}).

  @figure["fig:vector-ref-extension" "Syntax extension for array references"
    @codeblock{
      #lang typed/racket ;; vector-ref-extension.rkt
      (require
        (only-in racket/unsafe/ops unsafe-vector-ref)
        (for-syntax racket/base syntax/parse))

      (define-syntax (-vector-ref stx)
        (syntax-parse stx
         [(_ #(e* ...) i)
          #:when (integer? (syntax->datum #`i))
          ;; Case 1: constant args
          (define v-val (syntax->datum #`(e* ...)))
          (define i-val (syntax->datum #`i))
          (if (< -1 i-val v-val)
            ;; then S-RefPass
            #`(unsafe-vector-ref '#(e* ...) i)
            ;; else S-RefFail
            (error 'vector-ref "~a ~a" v-val i-val))]
         [(_ args ...)
          ;; Case 2: S-Ref
          #`(vector-ref args ...)]))

      (provide (rename-out [-vector-ref vector-ref]))
    }
  ]

First, the @racket[require] statement imports the @racket[unsafe-vector-ref]
 function to the runtime environment and the libraries @racket[racket/base]
 and @racket[syntax/parse] to the compile-time environment.
We name the extension @racket[-vector-ref] to avoid shadowing the uses of
 Typed Racket's @racket[vector-ref] in Case 2 of the extension.
At the end of the module, the @racket[provide] statement renames @racket[-vector-ref]
 to replace the default @racket[vector-ref] in importing modules.

Code within @racket[-vector-ref] is evaluated at compile-time to
 transform all calls and uses of the extension in an importing module.
For instance, calling @racket[(vector-ref x 4)] will invoke
 @racket[-vector-ref] with the expression @racket[(vector-ref x 4)] bound to the
 formal parameter @racket[stx].
    @; A higher-order use like @racket[(map vector-ref vs is)] will invoke @racket[-vector-ref]
    @;  with @racket[stx] bound to the identifier @racket[vector-ref] before evaluating
    @;  the call to @racket[map].
In any event, the first task of @racket[-vector-ref] is to destructure its argument
 @racket[stx] using the @racket[syntax-parse] form.
We consider three cases.
The first matches expressions with three elements, like @racket[(vector-ref #(0) 0)],
 where the first element is anything, the second is a vector literal, and the third
 is an integer.
Integer literals are recognized by the @racket[#:when] clause, which extracts
 the value from a @emph{pattern variable} @racket[i] and tests whether this value
 is an integer.
If this first match is successful, then we implement the @sc{S-RefPass}
 and @sc{S-RefFail} rules by comparing the value of the integer literal
 contained in @racket[i] against the number of elements captured by the zero-or-more
 pattern @racket[(e ...)].
When the reference is in bounds, we use the constructor @|stx|
 to produce code.@note[@elem{If it helps, you can mentally replace all @|stx|
   with the arrow @exact{$\elabarrow$} from our model.}]
The second and third cases are simpler.
The second elaborates any call to @racket[-vector-ref] into a @racket[vector-ref]
 call---even calls made with zero or seven arguments.
We let the type checker deal with such erroneous cases.
    @; The third case replaces higher-order calls of @racket[-vector-ref] with
    @;  higher-order calls to Typed Racket's @racket[vector-ref] function.

Existing programs can use the extension by adding a 1-line import statement.

  @codeblock{
    #lang typed/racket
    (require vector-ref-extension)
    ....
  }

The import shadows @racket[vector-ref] from the @racket[typed/racket] language,
 replacing all occurrences with calls to our syntax extension.
As Typed Racket processes the code abbreviated as @racket[....] above,
 each vector reference is expanded to either a Typed Racket (checked)
 @racket[vector-ref], an @racket[unsafe-vector-ref], or a compile-time @racket[error].


@; -----------------------------------------------------------------------------
@section[#:tag "sec:segfault:evaluation"]{Evaluation}

@(let* ([vrx #rx"\\(vector-ref "]
        [vr-file* (for/list ([fn (sequence-append
                                   (in-glob "benchmark/vector/*.rkt")
                                   (in-glob "benchmark/vector/*/*.rkt"))]
                             #:when (with-input-from-file fn
                                      (lambda ()
                                        (regexp-match vrx (current-input-port)))))
                    fn)]
        ;; : (Listof (List Symbol Num-Hit Num-Miss))
        [all-file+optz* (with-cache "vector-ref-optz"
                                    (lambda ()
                                      (profile-point "counting vector-ref optz")
                                      (let-values ([(_in _out) (make-pipe)])
                                        (parameterize ([current-output-port _out])
                                          (displayln "(")
                                          (for-each collect-and-summarize vr-file*)
                                          (displayln ")"))
                                        (close-output-port _out)
                                        (let ([v (read _in)])
                                          (close-input-port _in)
                                          (begin0
                                            (for/list ([d (in-list v)])
                                              (cons (car d)
                                                    (or (for/first ([kvvv (in-list (cdr d))]
                                                                    #:when (eq? (car kvvv) 'vector-ref))
                                                          (list (cadr kvvv) (caddr kvvv)))
                                                        (list 0 0))))
                                            (profile-point "done w/ vector-ref optz")))))
                                    #:read (lambda (f+o*)
                                             (and
                                               (for/and ([f+o (in-list f+o*)]
                                                         [fn (in-list vr-file*)])
                                                 (eq? (car f+o) (string->symbol fn)))
                                               f+o*)))]
        [file+optz* (filter (compose1 positive? cadr) all-file+optz*)]
        [hit-count (length file+optz*)]
        [miss-count (- (length all-file+optz*) hit-count)]
        [num-optz (for/sum ([f+t (in-list file+optz*)]) (cadr f+t))])
        ;; -- for missed optz, take `caddr` of element of `file+optz*`
  @list[
  @figure["fig:vector-table" @elem{Summary of @racket[vector-ref] evaluation}
    @graphical-summary[
      #:hit-count    hit-count
      #:miss-count   miss-count
      #:bar-data     (for/list ([fhm (in-list (sort file+optz* < #:key cadr))])
                       (define h (cadr fhm))
                       (define m (caddr fhm))
                       (list (format "~a" h) (* 100 (/ h (+ h m)))))
      #:bar-title    "Percent of Array References Optimized"
      #:bar-x        (format "# Optimized refs by module")  ; (~a total)" num-optz
      #:bar-y        "%"
      #:y-max        100
    ]
  ]
  @elem{
  The above implementation works for toy examples, but useful programs often
   give constant values a name.
  For example, the implementation of @racket[gzip] used in the main Racket distribution
   declares array constants to implement a Huffman tree and heap.
  To accomodate this and similar idioms, we added syntax extensions for binding
   data to identifiers and constant folding for arithmetic operations.
  These additions are justified in @Secref{sec:define}, but they are straightforward
   extensions of the technique described in this section.

  With name-tracking, we were able to optimize
   two static array references in the @racket[gzip] implementation,
   one in @racket[hangman],
   12 references in a @racket[minesweeper] game, and
   480 references in an implementation of @racket[parcheesi].
  All three programs follow a general pattern of declaring a fixed-size vector
   in the scope of a module and implementing helper functions to manipulate the vector.
  The unusual success of @racket[parcheesi] was due to an inlining macro that
   would unroll a loop over a vector of pawns to straight-line array
   references at constant offsets.

  These successes are modest,@note{Especially since we analyzed over 120 files using
                                   arrays for places to optimize.}
   but encouraging since the target audience for these
   extensions are script writers.
  In prototype and throwaway code, we expect programmers to use more constant
   values and make more index errors.
  Our extensions catch some of these errors without imposing any annotation burden
   or increasing compile times --- in the files we analyzed, we observed no
   statistically significant difference compiling with and without our extensions
   enabled.
}])
