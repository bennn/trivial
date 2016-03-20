#lang scribble/manual
@require[racket/include]
@require[scribble/eval]
@require[scriblib/footnote]

@title[#:tag "top"]{Trivial: Solving the easiest type-checking problems}
@author[@hyperlink["https://github.com/bennn"]{Ben Greenman}]

@defmodule[trivial]
@(define trivial-eval (make-base-eval #:lang 'typed/racket/base '(begin (require trivial))))

This library exports a collection of @hyperlink["http://www.greghendershott.com/fear-of-macros/"]{macros} for improved standard library functions.
All exported macros are named with a trailing colon (meant as a hint that some extra type-checking may happen at the call site).@note{
  Not happy with the colon convention? @racket[trivial/no-colon] provides the same identifiers, colon-free.
  Same goes for each sub-collection, for instance you can require @racket[trivial/math/no-colon].
}


@emph{Hidden Agenda:}
Macros are good for both (1) @emph{syntax transformations} and (2) @emph{syntax analysis}.
We use these powers to derive type constraints and convince Typed Racket that the end result is well-typed.


Each section below describes one source file.
These files may be @racket[require]-d individually, or all at once using @racket[(require trivial)].


@section{Format Strings}
@defmodule[trivial/format]

Racket's @racket[format] function expects a template string and a few extra arguments.
The extra arguments must match the sequence of @hyperlink["http://docs.racket-lang.org/reference/Writing.html#%28def._%28%28quote._~23~25kernel%29._fprintf%29%29"]{formatting escapes} in the template string.

The arguments are normally validated at runtime, but our @racket[format:] macro
 will check arity and argument types at compile-time, if the format string is a constant.

@defform[(format: s e* ...)]{
  If the sequence of format directives in @racket[s] are known, generate constraints on the extra arguments @racket[e*].
  There must be one element in @racket[e*] for each format escape, and argument types must satisfy the type constraint of their matching format escape.

  If @racket[s] is not a string literal, do not perform static checks.
  Instead, desugar to the @racket[format] function from @racket[racket/base].
}

@examples[#:eval trivial-eval
  (format: "hello, ~a" 'world)
  (format: "hello, ~a" 'too "many" 'args)
  (format: "binary = ~b" 3.14)
  (let: ([fmt "non-binary = ~b"])
    (format: fmt "Type Error!"))
]


@section{Regular Expression Matching}
@defmodule[trivial/regexp]

Typed Racket's type for @racket[regexp-match] is conservative.
No matter how many groups are in the regular expression pattern, succesful matches always have the type @racket[(Pairof String (Listof (U #f String)))].
In other words, the result is one string for the matched substring and an unknown number of strings (or @racket[#f] values) representing the matched groups.


@defform[(regexp-match: s e* ...)]{
  Analyze the pattern string @racket[s], if available during compilation.
  Verify that all groups in @racket[s] are valid (no mismatched parentheses).
  Additionally coerce the resulting list of matches to have type @racket[(List String ...)] on success, where the length of the list equals the number of groups.
}

@examples[#:eval trivial-eval
  (ann (regexp-match: "rack" "racket")
       (U #f (List String)))
  (ann (regexp-match: #rx"r([ack]*)" "racket")
       (U #f (List String String)))
  (ann (regexp-match: #"(la(m*)bda)" #"lam")
       (U #f (List Bytes Bytes Bytes)))
  (regexp-match: "(bad))group" "")
]

  @emph{Note:} the regular expression @racket{|} operator is not currently supported because it can nullify some groups.
  Using the @racket{|} will result is a less-precise type.
  Same goes for groups followed by a Kleene star.


@examples[#:eval trivial-eval
  (ann (regexp-match: #rx"ri(g*)ht" "right")
       (U #f (List String String)))
  (ann (regexp-match: #rx"left|ri(g*)ht" "riggght")
       (U #f (List String String)))
  (ann (regexp-match: #rx"(a)*(b)" "bbb")
       (U #f (List String String String)))
]

Large regular expression patterns are expensive to compile, so we offer a definition form that remembers the number of groups in a pattern for later calls to @racket[regexp-match:].

@defform*[((regexp: id pattern)
           (pregexp: id pattern)
           (byte-regexp: id pattern)
           (byte-pregexp: id pattern))]{}

@defform*[((define-regexp: ([id expr] ...) expr ...)
           (define-pregexp: ([id expr] ...) expr ...)
           (define-byte-regexp: ([id expr] ...) expr ...)
           (define-byte-pregexp: ([id expr] ...) expr ...)
           (let-regexp: ([id expr] ...) expr ...)
           (let-pregexp: ([id expr] ...) expr ...)
           (let-byte-regexp: ([id expr] ...) expr ...)
           (let-byte-pregexp: ([id expr] ...) expr ...))]{}



@section{Mathematical Operators}
@defmodule[trivial/math]

Typed Racket has special types for @racket[Zero] and @racket[One].
The special cases are often convenient, but can lead to confusing behavior.@note{See @hyperlink["https://groups.google.com/forum/#!searchin/racket-users/klaus/racket-users/BfA0jsXrioo/yFhMLkl_AQAJ"]{this email thread} for some confusion.}

@examples[#:eval trivial-eval
  (ann (- 1 1) Zero)
  (ann (- 2 2) Zero)
]

The following operators special-case all numeric constants, uniformly sweeping the problem under a rug.

@defform*[((+: e* ...)
           (-: e* ...)
           (*: e* ...)
           (/: e* ...)
           (expt: e1 e2))]{
  Similar to the corresponding @racket[racket/base] operators, but reduces every pair of numeric constants in @racket[e* ...] during @emph{macro expansion}.
}

@examples[#:eval trivial-eval
  (ann (-: 1 1) Zero)
  (ann (-: 2 2) Zero)
  (ann (-: 99 (+: 9 (*: 9 10))) Zero)
  (ann (+: 1 (/: 1 3)) 4/3)
]

Only constants are reduced.
Any non-constants will upset the static analysis.

@examples[#:eval trivial-eval
  (lambda ([n : Integer])
    (ann (-: n 2) Zero))
]

However, an expression such as @racket[(+ 1 2 n)] will compile to @racket[(+ 3 n)].


@section{Arity Awareness}
@defmodule[trivial/function]

When the arity of a function @racket[f] is known, we replace calls
 to generic functions like @racket[curry] with specialized versions.

@defform*[((curry: f)
           (map: f e* ...))]{
  If @racket[f] accepts 2 arguments, @racket[(curry f)] returns a function
   of one argument @racket[x] which returns a function of one argument @racket[y]
   which returns @racket[(f x y)].

  Likewise, @racket[map f e* ...] expects 2 lists in place of @racket[e*].
}


@section{Sized Vectors}
@defmodule[trivial/vector]

These vector operations store and update a vector's length information.

@defform*[((vector: e* ...)
           (make-vector: i e)
           (build-vector: i e)
           (vector-length: v)
           (vector-ref: v i)
           (vector-set!: v i k)
           (vector-map f v* ...)
           (vector-map! f v* ...)
           (vector-append: v1 v2)
           (vector->list: v)
           (vector->immutable-vector: v)
           (vector-fill: v k)
           (vector-take: v i)
           (vector-take-right: v i)
           (vector-drop: v i)
           (vector-drop-right: v i))]{}

@examples[#:eval trivial-eval
  (vector-ref: (vector-append: '#(A) '#(B)) 0)
  (vector-ref: (vector-append: '#(A) '#(B)) 2)
]


@section{Binding forms}
@defmodule[trivial/define]

The @racket[let:] and @racket[define:] forms infer and propagate data about their
 bindings.

@examples[#:eval trivial-eval
  (let: ([n 0])
    (ann (/ 3 3) One))
  (let ([n 0])
    (ann (/ 3 3) One))
]


@section{Further Reading}

Short essay: @hyperlink["http://www.ccs.neu.edu/home/types/publications/letter-of-values/macro-goggles.pdf"]{here}.


@section{Credits}

Inspired by:
@itemlist[
@item{@hyperlink["http://www.ccs.neu.edu/home/samth/"]{Sam Tobin-Hochstadt}}
@item{@hyperlink["http://www.ccs.neu.edu/home/stchang/"]{Stephen Chang}}
@item{@hyperlink["http://www.ccs.neu.edu/home/matthias/"]{Matthias Felleisen}}
@item{@hyperlink["http://www.ccs.neu.edu/home/dherman/research/papers/icfp04-dsl-analysis.pdf"]{David Herman and Philippe Meunier}}
]

The views/code expressed in this library do not necessarily reflect the good taste and decency of the aforementioned sources of inspiration.
