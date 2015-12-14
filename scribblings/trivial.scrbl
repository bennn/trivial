#lang scribble/manual
@require[racket/include]
@require[scribble/eval]
@require[scriblib/footnote]

@title[#:tag "top"]{@bold{Trivial: Solving the easiest type-checking problems}}
@author[@hyperlink["https://github.com/bennn"]{Ben Greenman}]

@defmodule[trivial]
@(define trivial-eval (make-base-eval #:lang 'typed/racket/base '(begin (require trivial))))

This library exports a collection of @hyperlink["http://www.greghendershott.com/fear-of-macros/"]{macros} that implement statically-checked interfaces to standard library functions.
All exported macros are named with a trailing colon (meant as a hint that some extra type-checking may happen at the call site).


@emph{Hidden Agenda:}
Macros are good for both (1) @emph{syntax transformations} and (2) @emph{syntax analysis}.
We use these powers to (1) derive type constraints and (2) convince Typed Racket that the end result is well-typed.


Each section below describes one source file.
These files may be @racket[require]-d individually, or all at once using @racket[(require trivial)].


@section{Format Strings}
@defmodule[trivial/format]

Racket's @racket[format] function expects a template string and a few extra arguments.
The extra arguments must match the sequence of @hyperlink["http://docs.racket-lang.org/reference/Writing.html#%28def._%28%28quote._~23~25kernel%29._fprintf%29%29"]{formatting escapes} in the template string.

Normally, the arguments are validated at runtime.
We can do (slightly) better!

@defform[(format: s e* ...)]{
  If @racket[s] is a string literal, parse the format escapes from @racket[s] and use them to generate constraints on the extra arguments @racket[e*].
  There must be one element in @racket[e*] for each format escape, and argument types must satisfy the type constraint of their matching format escape.

  If @racket[s] is not a string literal, do not perform static checks.
  Instead, desugar to the @racket[format] function from @racket[racket/base].
}

@examples[#:eval trivial-eval
  (format: "hello, ~a" 'world)
  (format: "hello, ~a" 'too "many" 'args)
  (format: "binary = ~b" 3.14)
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
]

  @emph{Note:} the regular expression @racket{|} operator is not currently supported because it can nullify some groups.
  Using the @racket{|} will result is a less-precise type.


@examples[#:eval trivial-eval
  (ann (regexp-match: #rx"ri(g*)ht" "right")
       (U #f (List String String)))
  (ann (regexp-match: #rx"left|ri(g*)ht" "riggght")
       (U #f (List String String)))
]

Regular expression patterns are expensive to compile, so we offer a definition form that remembers the number of groups in a pattern for later calls to @racket[regexp-match:].

@defform*[((define-regexp: id pattern)
           (define-pregexp: id pattern)
           (define-byte-regexp: id pattern)
           (define-byte-pregexp: id pattern))]{}

@examples[#:eval trivial-eval
  (let ()
    (define-regexp: rx-address "([0-9]+) ([a-z]+) ")
    (ann (regexp-match: rx-address "1 main street")
         (U #f (List String String String))))
  (let ()
    (define-pregexp: px-age "([0-9][0-9][0-9]) years old$")
    (ann (regexp-match: px-age "forever young")
         (U #f (List String String))))
  (let ()
    (define-byte-regexp: bx-logmsg #rx#"^LOG: (.*)$")
    (ann (regexp-match: bx-logmsg #"LOG: full steam ahead")
         (U #f (List Bytes Bytes))))
  (let ()
    (define-byte-pregexp: bpx-red #px#"red")
    (ann (regexp-match: bpx-red #"blue")
         (U #f (List Bytes))))
]

The following expression forms also cache the number of pattern groups.
Use these instead of the standard defaults.

@defform*[((regexp: pattern)
           (pregexp: pattern)
           (byte-regexp: pattern)
           (byte-pregexp: pattern))]{}

@examples[#:eval trivial-eval
  (let ()
    (define-regexp: rx-email (regexp: "^(.*)@(.*)\\.com$"))
    (ann (regexp-match: rx-email "admin@internet.com")
         (U #f (List String String String))))
  (let ()
    (define-regexp: rxp (regexp "missing(.*)colon"))
    (ann (regexp-match: rxp "miss")
         (U #f (List String String))))
]


@section{Mathematical Operators}
@defmodule[trivial/math]

Typed Racket has special types for @racket[Zero] and @racket[One].
The special cases are often convenient, but can lead to confusing behavior.

@examples[#:eval trivial-eval
  (ann (- 1 1) Zero)
  (ann (- 2 2) Zero)
]

The following operators special-case all numeric constants, uniformly sweeping the problem under a rug.

@defform*[((+: e* ...)
           (-: e* ...)
           (*: e* ...)
           (/: e* ...))]{
  Similar to the corresponding @racket[racket/base] operators, but reduces every pair of numeric constants in @racket[e* ...] during @emph{macro expansion}.
}

@examples[#:eval trivial-eval
  (ann (-: 1 1) Zero)
  (ann (-: 2 2) Zero)
  (ann (-: 99 (+: 9 (*: 9 10))) Zero)
  (ann (+: 1 (/: 1 3)) 4/3)
  ((lambda ([f : (-> Natural Natural Natural)]) (f 1 1)) +:)
]

Again, only constants are reduced.
Any non-constants will upset the static analysis.

@examples[#:eval trivial-eval
  (let ([n 2])
    (ann (-: n 2) Zero))
]

We'll see if this is ever useful!


@section{Credits}

Inspired by:
@itemlist[
@item{@hyperlink["http://www.ccs.neu.edu/home/samth/"]{Sam Tobin-Hochstadt}}
@item{@hyperlink["http://www.ccs.neu.edu/home/matthias/"]{Matthias Felleisen}}
@item{@hyperlink["http://www.ccs.neu.edu/home/dherman/research/papers/icfp04-dsl-analysis.pdf"]{David Herman and Philippe Meunier}}
@item{The @hyperlink["http://fsl.cs.illinois.edu/images/5/5e/Cayenne.pdf"]{Cayenne} programming language}
]

The views and code expressed in this library do not necessarily reflect the good taste and decency of the aforementioned sources of inspiration.