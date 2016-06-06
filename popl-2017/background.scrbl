#lang scribble/sigplan @onecolumn

@require["common.rkt" pict racket/class racket/draw]

@title[#:tag "sec:background"]{If You Know What I Mean}
@; A poet should be of the
@; old-fahioned meaningless brand:
@; obscure, esoteric, symbolic,
@; -- the critics demand it;
@; so if there's a poem of mine
@; that you do understand
@; I'll gladly explain what it means
@; till you don't understand it.

@;    Computers understand a very limited set of instructions.
@;    It is tedious and error-prone to describe even simple thoughts in terms of these
@;     instructions, but programming languages provide abstraction mechanisms
@;     that can make the process bearable.
@;    For example, low-level or assembly languages can implement the following
@;     mathematical function for referencing an element of a fixed-length
@;     data structure:
@;    
@;      @exact|{\[
@;        \RktMeta{ref} = \lambda (\RktMeta{v}~\RktMeta{i})
@;          \left\{\begin{array}{l l}
@;            \RktMeta{v}_{i+1} & \RktMeta{if}~\RktMeta{v} = \vectorvn
@;          \\                 ~&\RktMeta{and}~\RktMeta{i} \in \mathbb{N}
@;                              ~\RktMeta{and}~i < n
@;          \\[4pt]
@;          \bot & \RktMeta{otherwise}
@;          \end{array}\right.
@;      \]}|
@;    
@;    @;Here we use the notation @exact|{$\langle \RktMeta{v}_0 \ldots \RktMeta{v}_{n-1} \rangle$}|
@;    @; to describe a vector of @exact{$n$} elements.
@;    Whether the implementation is a labeled sequence of instructions or a C function
@;     is not important; what matters is the precise specification
@;     and our ability to compute the function via machine.
@;    Memory-safe functions with clear semantics are the core building blocks for
@;     sane programs.
@;    
@;    As programs grow in size, complexity, and importance, it is desirable to have
@;     some guarantees about how a program will run before actually running the
@;     program.
@;    Ideally, we would statically prove that @racket[ref] is
@;     never applied to arguments that are not vectors or to out-of-bounds indices.
@;    @;Such a proof would imply that every call @racket[(ref v i)] in the program
@;    @; would yield a non-@|bot| value and help show that the overall program works as intended.
@;    But proving such properties is difficult and there are often many
@;     functions in a program for which we seek guarantees, so we settle for
@;     approximate results.
@;    Instead of statically ruling out all calls @racket[(ref v i)] that produce
@;     @|bot|, we strive to eliminate a large subset automatically.
@;    
@;    Type systems have emerged as a powerful and convenient way of statically detecting
@;     errors.
@;    Instead of tracking the flow of exact values through a program, a type system
@;     tracks the flow of @emph{types} denoting a range of possible runtime values.
@;    Depending on the type system and programmers' discipline using it, the range
@;     might be limited enough to show that @racket[ref] never returns @|bot|
@;     in a program; however, the tradeoff is always the time and energy programmers
@;     are willing to invest in writing down and maintaining type information.
@;    As it stands, the most widely used type systems are those that require
@;     minimal annotations from the programmer and catch only shallow, common errors.
@;    
@;    
@;    @section{Problem Statement and Promises}
@;    
@;    @let*[([all-w 150]
@;           [lang-w (/ all-w 2)]
@;           [lang-h 20]
@;           [lang-rectangle
@;            (lambda (brush-style
@;                     fill-color
@;                     #:width [lang-w lang-w] ;; getting lazy there ben
@;                     #:height [lang-h lang-h]
@;                     #:slant-left [slant-left #f]
@;                     #:border-color [maybe-border-color #f]
@;                     #:border-width [border-width 3])
@;              (dc (lambda (dc dx dy)
@;                (define old-brush (send dc get-brush))
@;                (define old-pen (send dc get-pen))
@;                (define border-color (or maybe-border-color fill-color))
@;                (send dc set-brush
@;                  (new brush% [style brush-style] [color fill-color]))
@;                (send dc set-pen
@;                  (new pen% [width border-width] [color border-color]))
@;                ;; --
@;                (define path (new dc-path%))
@;                (send path move-to 0 0)
@;                (if slant-left
@;                  (begin (send path line-to (- 0 slant-left) (/ lang-h 2))
@;                         (send path line-to 0 lang-h))
@;                  (send path line-to 0 lang-h))
@;                (send path line-to lang-w lang-h)
@;                (send path line-to lang-w 0)
@;                (send path close)
@;                (send dc draw-path path dx dy)
@;                ;; --
@;                (send dc set-brush old-brush)
@;                (send dc set-pen old-pen)) lang-w lang-h))]
@;           [all-e (rectangle all-w lang-h)]
@;           [hshim (blank (/ all-w 6) lang-h)]
@;           [untyped-e (hc-append hshim (lang-rectangle 'vertical-hatch "CornflowerBlue" #:border-width 4 #:height (+ lang-h 2)))]
@;           [typed-e (hc-append (lang-rectangle 'horizontal-hatch "Coral" #:border-width 2) hshim)]
@;           [reality-of-ml (rc-superimpose (lc-superimpose all-e untyped-e) typed-e)]
@;           [value-e
@;            (let ([s 'solid]
@;                  [c "LimeGreen"]
@;                  [w (- lang-w lang-h)]
@;                  [b 1])
@;              (hc-append (blank (* lang-h 0.8) lang-h)
@;                         (lang-rectangle s c #:width (/ lang-w 3) #:height (/ lang-h 3) #:border-width b)
@;                         (lang-rectangle s c #:border-width b)))]
@;           [reality-of-tr (lc-superimpose reality-of-ml value-e)]
@;           )]{@list{
@;      No matter how precise the type system, static analysis cannot recognize
@;       all correct programs and reject all incorrect programs.
@;      Layering a type system on top of an untyped programming language therefore
@;       leads to a world where the space of all syntactically valid programs
@;       (drawn as a white box, of course) is partitioned into two overlapping sets.
@;    
@;       @centered[reality-of-ml]
@;    
@;      On the left, outlined in blue with vertical hatches, we have the set of all untyped programs
@;       that produce meaningful results (read: non-error, non-diverging terms).
@;      On the right, outlined in orange with horizontal hatches, we have the set of all programs
@;       approved by the type system.
@;      Some type-correct programs raise runtime errors when evaluated, and some
@;       meaningful programs are rejected by the type checker.
@;    
@;      Again, the impossible goal is for the highlighted boxes to overlap.
@;      Our argument is that adding a @emph{syntactic elaboration} phase before
@;       typechecking can yield a language described by the
@;       solid green area shown below, capturing more of the meaningful untyped
@;       programs and fewer of the typed programs that go wrong, though rejecting
@;       some typed programs that run to completion but contain a subterm that would
@;       raise an exception if evaluated (Theorem @exact|{\ref{thm:relevance}}|).
@;    
@;       @centered[reality-of-tr]
@;    }}
@;    
@;    Starting from an untyped programming language built from a grammar of terms
@;     @exact{$e$} with a reduction relation @exact{$e \Downarrow v$}
@;     and a type judgment @exact{$\vdash e : \tau$} relating terms to
@;     values @exact{$v$} and types @exact{$\tau$}, we derive an extended language
@;     by inserting a function @exact{$\elaborate(e)$} before type checking.
@;    The extended language has a type system @exact{$\vdashe$} consisting of a single
@;     rule:
@;    
@;      @exact|{
@;        \begin{mathpar}
@;          \inferrule*{
@;            \vdash \elaborate(e) : \tau
@;          }{
@;            \vdashe e : \tau
@;          }
@;        \end{mathpar}
@;      }|
@;    
@;    We also derive a reduction relation on typed terms @exact{$\vdashe e : \tau$}
@;     as @exact{$\elaborate(e) \Downarrow v$}.
@;    Type soundness for @exact{$\vdashe$} is thus a corollary of
@;     type soundness for the @exact{$\vdash$} judgment.
@;    
@;    The output of @exact{$\elaborate(e)$} is a term @exact{$e'$},
@;     though @exact{$e'$} may be a labeled error term
@;     @exact|{$\bot^{e''}$}| meaning the elaborator detected an error in the program.
@;    Terms @exact{$e$} such that @exact{$\elaborate(e)$} type checks but @exact{$e$}
@;     does not are newly expressible in the extended system.
@;    Terms @exact{$e$} such that @exact{$\vdash e : \tau$} but
@;     @exact|{$\elaborate(e) = \bot^{e'}$}| are programs validated by the typechecker
@;     but rejected by the extended system because the subterm @exact{$e'$} of @exact{$e$}
@;     would raise a runtime error if evaluated.
@;    
@;    Our main contribution is defining a useful collection of @emph{local}
@;     transformations to perform during elaboration.
@;    Each local elaboration is proven correct in isolation.
@;    We then define @exact{$\elaborate(e)$} as the union of these local cases
@;     and a default elaboration that applies @exact{$\elaborate$} to all
@;     subterms of @exact{$e$}.
@;    
@;    To avoid reasoning about terms post-elaboration we will describe
@;     our local elaborations with type rules of the form: @todo{maybe drop this}
@;    
@;      @exact|{
@;        \begin{mathpar}
@;          \inferrule*{
@;            \prope(f, e)
@;            \\\\
@;            \vdash f : \tau_f
@;            \\
@;            \vdash e : \tau_e
@;          }{
@;            \vdashe f~e : \tau
@;          }
@;        \end{mathpar}
@;      }|
@;    
@;    where @exact{$\prope$} is a proposition about syntactic terms
@;     @exact{$f$} and @exact{$e$}
@;     used to justify the transformation @exact|{$\elaborate(f~e)$}|.
@;    Despite this inference rule shorthand, elaborations and @exact{$\prope$}
@;     conditions run before typechecking and have no access to type information
@;     in the program.
@;    
@;    The relationship between terms @exact{$e$} and @exact{$\elaborate(e)$} is given
@;     by three theorems: @todo{see next section for now}.
@;    
@;    Correct elaborations obey these theorems.
@;    @; Note: there is no guarantee that elaboration produces a well-typed term
@;    @;  because calls like @exact{$\elaborate($}@racket[ref 0 0]@exact{$)$} are allowed.
@;    
@;    @; We refrain from making stronger statements about elaborate because
@;    @; real systems will have lots of other elaborations.


@section{Syntax Extensions}

Syntax extension systems have been incorporated in
 a number of mainstream programming languages.
Essentially, syntax extensions let the programmer write code that generates code.
In practice this gives programmers the ability to extend the syntax of their
 language, abstract over textual patterns, and control the evaluation order
 of expressions.

Racket's syntax extension API is particularly mature and has inspired
 similar APIs in at least two other languages, so we adopt it here to introduce
 syntax extensions.
As a first example, suppose we want to assert that expressions
 @racket[e_1], @racket[e_2], @racket[e_3], and @racket[e_4] all evaluate to
 the same value.
By hand, we might write a sequence of unit tests:
@racketblock[
  (begin (check-equal? e_2 e_1)
         (check-equal? e_3 e_1)
         (check-equal? e_4 e_1))
]
but these tests follow a simple pattern that we can abstract as a @emph{syntax rule},
 using ellipses (@racket[...]) to capture an arbitrary number of expressions.
@racketblock[
  (define-syntax-rule (check-all-equal? e_1 e_rest ...)
    (begin (check-equal? e_rest e_1) ...))
]
Our tests can now be written more concisely:
@racketblock[
  (check-all-equal? e_1 e_2 e_3 e_4)
]
making them easier to read and maintain.
Moreover, we can easily improve the rule to evaluate @racket[e_1] only once
 instead of @math{n-1} times:
@racketblock[
  (define-syntax-rule (check-all-equal? e_1 e_rest ...)
    (let ([v e_1])
      (check-equal? e_rest v) ...))
]

Intuitively, syntax rules perform a search-and-replace before the program
 is compiled; however, the replacement process is careful to preserve the
 lexical structure of programs.
For instance, a program that uses the same variable name @racket[v] as the syntax rule:
@racketblock[
  (define v 5)
  (check-all-equal? (+ 2 2) v)
]
will expand to code that introduces a fresh variable @racket[v1]:
@racketblock[
  (define v 5)
  (let ([v1 (+ 2 2)])
    (check-equal? v v1))
]
to avoid shadowing the programmer's variable with the identifier used inside
 the syntax rule.
Details on how this so-called @emph{hygenic} expansion is implemented and
 its benefits to extension writers and users are explained by Flatt@~cite[f-popl-2016 fcdb-jfp-2012].

In addition to pattern-based syntax rules, one can extend Racket with
 arbitrary functions defined over @emph{syntax objects}.
For instance, we can write an extension that formats log messages to a port
 @racket[log-output] at runtime when the program is compiled with a flag
 @racket[DEBUG] enabled. If the flag is disabled, we perform a no-op.
The expansion of @racket[log] calls happens during compilation:
@(begin
#reader scribble/comment-reader
@codeblock|{
  ;; With DEBUG enabled
  (log "everything ok")
  ==> (displayln "everything ok" log-output)
  ;; With DEBUG disabled
  (log "everything still ok")
  ==> (void)
}|)

The @racket[syntax-parse] form is a convenient way to implement @racket[log].
In this case, we use @racket[syntax-parse] to deconstruct a syntax object
 @racket[stx] representing an application of @racket[log].
The output of syntax parse is a new syntax object built using the constructor
 @racket[syntax].
@codeblock{
  (define-syntax (log stx)
    (syntax-parse stx
     [(log message)
      (if DEBUG
        (syntax (displayln message log-output))
        (syntax (void)))]))
}
We can further enhance our implementation with a compile-time check that
 the value bound to the pattern variable @racket[message] is a string literal.
@codeblock{
  (define-syntax (log stx)
    (syntax-parse stx
     [(log message)
      (unless (string? (syntax->datum (syntax message)))
        (error "log: expected a string literal"))
      (if DEBUG
        (syntax (displayln message log-output))
        (syntax (void)))]))
}
@; Alternatively, we can use Racket's @emph{syntax classes} to the same effect.
@; The @racket[str] syntax class recognizes literal strings.
@; Binding it to the pattern variable @racket[message] causes calls like
@;  @racket[(log 61)] to raise a compile error.
@; @codeblock{
@;   (define-syntax (log stx)
@;     (syntax-parse stx
@;      [(log message:str)
@;       (if DEBUG
@;         (syntax (displayln message log-output))
@;         (syntax (void)))]))
@; }
With this enhancement, calls like @racket[(log 61)] are rejected statically.
Unfortunately, arguments that @emph{evaluate} to string literals
 are also rejected
 because the syntax extension cannot statically predict what value an arbitrary
 expression will reduce to.
This is a fundamental limitation, but we can make a small improvement by accepting
 any @racket[message] produced by another (trusted) syntax extension.
Suppose @racket[++] is an extension for concatenating two strings.
If we assign a unique @emph{syntax property} to the syntax object produced
 by @racket[++], we can later retrieve the property in the @racket[log] extension.

First, we give an implementation of @racket[++] in terms of Racket's
 built-in @racket[string-append] function, crucially using @racket[syntax-property]
 to associate the value @racket['string] with the key @racket['static-type].
@codeblock{
  (define-syntax (++ stx)
    (syntax-parse stx
     [(++ s1 s2)
      (syntax-property
        (syntax (string-append s1 s2))
        'static-type 'string)]))
}
Assuming now that the key @racket['static-type] accurately describes the value
 contained in a syntax object, @racket[log] can accept both string
 literals and tagged syntax objects.
@codeblock{
  (define-syntax (log stx)
    (syntax-parse stx
     [(log message)
      (define is-string?
        (string? (syntax->datum (syntax message))))
      (define expands-to-string?
        (eq? 'string
             (syntax-property
               (local-expand message)
               'static-type)))
      (unless (or is-string? expands-to-string?)
        (error "log: expected a compile-time string"))
      (if DEBUG
        (syntax (displayln message log-output))
        (syntax (void)))]))
}

Our syntax extensions @racket[check-all-equal?], @racket[log], and @racket[++]
 are indistinguishable from user-defined functions or core language forms,
 yet they perform useful transformations before a program is typechecked or
 compiled.
This seamless integration gives Racket programmers the ability to grow the
 language and tailor it to their specific needs.

@; NOTE: we will use similar 'typed macros' in a coming section... right?
