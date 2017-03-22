#lang scribble/manual
@require[
  scribble/example
  (for-label
    racket/base
    racket/contract
    racket/function
    (only-in typed/untyped-utils syntax-local-typed-context?))
]

@(define (make-typed-eval) (make-base-eval #:lang 'typed/racket '(begin (require trivial))))

@; =============================================================================
@title[#:tag "ttt:tutorial"]{Using Tailorings}

The following tailorings are all provided by the @racketmodname[trivial] module.
Note that these tailorings have the same name as their Racket and Typed Racket equivalents to make this library a drop-in replacement for existing code.

The descriptions below assume familiarity with the Racket reference and describe only the new behavior of the tailored function or form.
Click the name of any tailoring to see its definition in the Racket reference.

@section{Built-in Tailorings}

@subsection{Definitions}
@defmodule[trivial/define]

@bold{WARNING} the static analysis implemented by @racket[trivial/define] is unsound in the presence of @racket[set!].
Do not @racket[set!] in a module that uses @racket[trivial/define].

@deftogether[(
  @defform[#:id define (define id expr)]{}
  @defform/none[(define (head args) expr)]{}
)]{
  Forces local expansion of @racket[expr] and associates any inferred properties with @racket[id] for the rest of expansion.
}

@deftogether[(
  @defform[#:id let (let ([id val-expr] ...) body ...+)]{}
  @defform[#:id let* (let* ([id val-expr] ...) body ...+)]{}
)]{
  Forces local expansion of each @racket[val-expr] and associates any inferred properties with the respective @racket[id] in the context of @racket[body ...+].
}

@subsection{Format Strings}
@defmodule[trivial/format]

@deftogether[(
  @defproc[(format [form string?] [v any/c] ...) string?]{}
  @defproc[(printf [form string?] [v any/c] ...) void?]{}
)]{
  When the string @racket[form] is available during expansion, checks the number of values @racket[v ...] against the number of formatting escapes in @racket[form] that require arguments.
  When used in a Typed Racket module, annotates each @racket[v] with the type required by the corresponding formatting escape.
}


@subsection{Functions}
@defmodule[trivial/function]

@defform[#:id curry (curry f)]{
  This form does not have a Typed Racket equivalent.

  When the arity of the procedure @racket[f] is available during expansion, expands to a curried version of @racket[f].
  In other words, if @racket[f] is a function of @racket[N] arguments then @racket[(curry f)] is a chain of @math{N} one-argument functions.

  For example,
  @racketblock[
    (curry (λ (x y z) (+ (* x y) z)))
  ]

  behaves the same as:
  @racketblock[
    (λ (x)
      (λ (y)
        (λ (z)
          (+ (* x y) z))))
  ]
}

@deftogether[(
  @defform[#:id lambda (lambda (id ...) body)]{}
  @defform[#:id λ (λ (id ...) body)]{}
)]{
  Expands to an anonymous function with known arity.
  Other tailorings can access this arity.
}


@subsection{Integer Arithmetic}
@defmodule[trivial/integer]

@deftogether[(
  @defproc[(+ [n integer?] ...) integer?]{}
  @defproc[(- [n integer?] ...) integer?]{}
  @defproc[(* [n integer?] ...) integer?]{}
)]{
  Constant-folding arithmetic functions.
  When all arguments @racket[n ...] have integer values available during expansion, expands to a constant integer (or bignum).
  When only some arguments have available values, reduces the expression accordingly.
}

@deftogether[(
  @defproc[(/ [n integer?] ...) real?]{}
  @defproc[(quotient [n integer?] ...) integer?]{}
)]{
  Constant-folding division.
  Raises a syntax error @racket{division by zero} when the final argument is @racket[zero?] during expansion.
}

@deftogether[(
  @defproc[(add1 [n integer?]) integer?]{}
  @defproc[(sub1 [n integer?]) integer?]{}
)]{
  Increment and decrement functions that propagate the value of their argument.
}

@defproc[(expt [n1 integer?] [n2 integer?]) integer?]{
  Constant-folding exponentiation.
  If the value of @racket[n1] is unknown, checks whether the value of @racket[n2] is @racket[zero?] or a small constant.
  In the latter case, unfolds to repeated multiplication of @racket[n1].
}


@subsection{List Operations}
@defmodule[trivial/list]

@deftogether[(
  @defproc[(make-list [k exact-nonnegative-integer?] [v any/c]) list?]{}
  @defproc[(build-list [k exact-nonnegative-integer?] [proc (-> exact-nonnegative-integer? any/c)]) list?]{}
  @defproc[(cons [a any/c] [d any/c]) pair?]{}
  @defproc[(car [p pair?]) any/c]{}
  @defproc[(cdr [p pair?]) any/c]{}
  @defproc[(list [v any/c] ...) list?]{}
  @defproc[(length [lst list?]) exact-nonnegative-integer?]{}
  @defproc[(list-ref [lst pair?] [pos exact-nonnegative-integer?]) any/c]{}
  @defproc[(list-tail [lst pair?] [pos exact-nonnegative-integer?]) any/c]{}
  @defproc[(append [lst list?] ...) list?]{}
  @defproc[(reverse [lst list?]) list?]{}
  @defproc[(map [proc procedure?] [lst list?] ...) list?]{}
  @defproc[(sort [lst list?] [less-than? (-> any/c any/c any/c)]) list?]{}
)]{
  Length-aware and content-aware list operations.
  Operations that build lists propagate the length of their arguments.
  Operations that access lists check for bounds errors and propagate information about cells within a list.

  Higher-order list functions check the arity of their functional argument; in particular, @racket[map] includes a static check that the arity of its first argument includes the number of lists supplied at the call-site.

  These Typed Racket examples demonstrate terms that would not typecheck without the @racketmodname[trivial] library.

  @examples[#:eval (make-typed-eval)
    (ann (- (length '(1 2 3)) 3) Zero)
    (ann (list-ref (make-list 5 0) 2) Zero)
    (ann (list-ref (list-ref '((A)) 0) 0) 'A)
  ]
}


@subsection{Regular Expressions}
@defmodule[trivial/regexp]

@deftogether[(
  @defproc[(regexp [str string?]) regexp?]{}
  @defproc[(pregexp [str string?]) pregexp?]{}
  @defproc[(byte-regexp [byt bytes?]) byte-regexp?]{}
  @defproc[(byte-pregexp [byt bytes?]) byte-pregexp?]{}
)]{
  Regexp constructors; when their argument value is known during expansion, these constructors record the number of groups specified by the argument.
}

@defproc[(regexp-match [pattern (or/c string? bytes? regexp? byte-regexp?)] [input (or/c string? bytes? path? input-port?)])
         (if (and (or (string? pattern) (regexp? pattern))
                  (or (string? input) (path? input)))
           (or/c #f (cons/c string? (listof (or/c string? #f))))
           (or/c #f (cons/c bytes?  (listof (or/c bytes?  #f)))))]{
  When possible, the type of the result list (in the case of a successful match) matches the number of groups in @racket[pattern].

  @margin-note{This example is adapted from @racketmodname[scribble/html-render]}
  @examples[#:eval (make-typed-eval)
      (: parse-font-size : String -> (List String String (U #f String) String))
      (define (parse-font-size str)
        (or (regexp-match #rx"^([0-9]*\\.?([0-9]+)?)(em|ex|pt|%|)$" str)
            (error 'malformed-input)))
  ]
}


@subsection{String Operations}
@defmodule[trivial/string]

@deftogether[(
  @defproc[(string-length [str string?]) exact-nonnegative-integer?]{}
  @defproc[(string-ref [str string?] [k exact-nonnegative-integer?]) char?]{}
  @defproc[(substring [str string?] [start exact-nonnegative-integer?] [end exact-nonnegative-integer? (string-length str)]) string?]{}
  @defproc[(string-append [str string?] ...) string?]{}
)]
@deftogether[(
  @defproc[(bytes-length [bstr bytes?]) exact-nonnegative-integer?]{}
  @defproc[(bytes-ref [bstr bytes?] [k exact-nonnegative-integer?]) byte?]{}
  @defproc[(subbytes [bstr bytes?] [start exact-nonnegative-integer?] [end exact-nonnegative-integer? (bytes-length bstr)]) bytes?]{}
  @defproc[(bytes-append [bstr bytes?] ...) bytes?]{}
)]{
  String and byte string operations that track the value of their arguments.

  @examples[#:eval (make-typed-eval)
    (regexp-match (string-append "(" "a*" ")") "aaa")
  ]
}


@subsection{Vector Operations}
@defmodule[trivial/vector]

@deftogether[(
  @defproc[(vector [v any/c] ...) vector?]{}
  @defproc[(make-vector [k exact-nonnegative-integer?] [v any/c]) vector?]{}
  @defproc[(build-vector [k exact-nonnegative-integer?] [proc (-> exact-nonnegative-integer? any/c)]) vector?]{}
  @defproc[(vector-append [vec vector?] ...) vector?]{}
  @defproc[(vector-ref [vec vector?] [pos exact-nonnegative-integer?]) any/c]{}
  @defproc[(vector-length [vec vector?]) exact-nonnegative-integer?]{}
  @defproc[(vector-set! [vec vector?] [pos exact-nonnegative-integer?] [v any/c]) vector?]{}
  @defproc[(vector-map [proc procedure?] [vec vector?] ...) vector?]{}
  @defproc[(vector-map! [proc procedure?] [vec vector?] ...) void?]{}
  @defproc[(vector->list [vec vector?]) list?]{}
  @defproc[(vector->immutable-vector [vec vector?]) vector?]{}
  @defproc[(vector-fill! [vec vector?] [v any/c]) void?]{}
  @defproc[(vector-take [vec vector?] [pos exact-nonnegative-integer?]) vector?]{}
  @defproc[(vector-take-right [vec vector?] [pos exact-nonnegative-integer?]) vector?]{}
  @defproc[(vector-drop [vec vector?] [pos exact-nonnegative-integer?]) vector?]{}
  @defproc[(vector-drop-right [vec vector?] [pos exact-nonnegative-integer?]) vector?]{}
)]{
  Length-aware and content-aware vector operations.
}


@section{Typed / Untyped Interaction}

@; @margin-note{The implementation is a little ugly, but it works for now.}
The macros provided by @racketmodname[trivial] and related submodules are all untyped, but should work @bold{with no problems} in Typed Racket modules.
Under the hood, these macros keep two copies of every tailored identifier and use @racket[syntax-local-typed-context?] to choose the appropriate identifiers and whether to expand to type-annotated code.

