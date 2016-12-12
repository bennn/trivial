#lang scribble/manual
@require[
  (for-label
    racket/base
    racket/contract)
]

@; =============================================================================
@title[#:tag "ttt:tutorial"]{Using Tailorings}

The following tailorings are all provided by the @racketmodname[trivial] module.
Note that these tailorings have the same name as their Racket and Typed Racket equivalents to make this library a drop-in replacement for existing code.

The descriptions below assume familiarity with the Racket reference and describe only the new behavior of the tailored function or form.
Click the name of any tailoring to see its definition in the Racket reference.

@defmodule[trivial/define]

@bold{WARNING} the static analysis implemented by @racket[trivial/define] is unsound in the presence of @racket[set!].
Do not @racket[set!] in a module that uses @racket[trivial/define].

@deftogether[(
  @defform[(define id expr)]{}
  @defform[(define (head args) expr)]{}
)]{
  Forces local expansion of @racket[expr] and associates any inferred properties with @racket[id] for the rest of expansion.
}

@deftogether[(
  @defform[(let ([id val-expr] ...) body ...+)]{}
  @defform[(let* ([id val-expr] ...) body ...+)]{}
)]{
  Forces local expansion of each @racket[val-expr] and associates any inferred properties with the respective @racket[id] in the context of @racket[body ...+].
}

@defmodule[trivial/format]

@deftogether[(
  @defproc[(format [form string?] [v any/c] ...) string?]{}
  @defproc[(printf [form string?] [v any/c] ...) void?]{}
)]{
  When the string @racket[form] is available during expansion, checks the number of values @racket[v ...] against the number of formatting escapes in @racket[form] that require arguments.
  When used in a Typed Racket module, annotates each @racket[v] with the type required by the corresponding formatting escape.
}


@defmodule[trivial/function]

@defform[(curry f)]{
  This form does not have a Typed Racket equivalent.

  When the arity of the procedure @racket[f] is available during expansion, expands to a curried version of @racket[f].
  In other words, if @racket[f] is a function of @racket[N] arguments then @racket[(curry f)] is a chain of @math{N} one-argument functions.
}

@deftogether[(
  @defform[(lambda (id ...) body)]{}
  @defform[(λ (id ...) body)]{}
)]{
  Expands to an anonymous function with known arity.
  Other tailorings can access this arity.
}


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
  Constant-folding increment and decrement.
}

@defproc[(expt [n1 integer?] [n2 integer?]) integer?]{
  Constant-folding exponentiation.
  If the value of @racket[n1] is unknown, checks whether the value of @racket[n2] is @racket[zero?] or a small constant.
  In the latter case, unfolds to repeated multiplication of @racket[n1].
}


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
}


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
}


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

The macros provided by @racketmodname[trivial] and related submodules are all untyped, but should work @bold{with no problems} in Typed Racket modules.
Under the hood, these macros keep two copies of every tailored identifier and use @racket[syntax-local-typed-context?] to choose the appropriate identifiers (and whether to expand to type-annotated code).
(The implementation is a little ugly, but it should work!)

