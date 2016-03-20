trivial
===
[![Build Status](https://travis-ci.org/bennn/trivial.svg)](https://travis-ci.org/bennn/trivial)
[![Coverage Status](https://coveralls.io/repos/bennn/trivial/badge.svg?branch=master&service=github)](https://coveralls.io/github/bennn/trivial?branch=master)
[![Scribble](https://img.shields.io/badge/Docs-Scribble-blue.svg)](http://pkg-build.racket-lang.org/doc/trivial/index.html)

This [Typed Racket](http://docs.racket-lang.org/ts-reference/) library provides "smarter" versions of standard library functions.
For example:

```
#lang typed/racket/base

(require trivial)

(printf: "hello, ~a")

;; > raco make test.rkt
;; format:: arity mismatch;
;;  the expected number of arguments does not match the given number
;;    expected: 1
;;      given: 0
```

The `printf:` (with a colon) checks whether its first argument is a string literal.
If so, it parses the string's format sequences for arity and type constraints.

Definitions work too:

```
(require trivial/no-colon)

(let ([s "hello ~a"])
  (printf s))

```


When the first argument to `printf:` is not clear from the program syntax, nothing special happens; we just call the standard `printf`.

```
#lang typed/racket/base

(require trivial)

((lambda ([s : String])
  (printf: s)) ;; Non-trivial!
 "hello, ~a\n")

;; Compiles successfully, but throws arity error at runtime
```

Besides `printf:`, this library also provides [macros](http://www.greghendershott.com/fear-of-macros/)
for:

- `regexp-match:`, to count the number of groups in a pattern and give the match result a more specific type
- constant-folding arithmetic, so `(/ 1 0)` raises a compile-time error
- arity-aware functions, like generalized `curry` and `map`
- size-aware vector functions, like length-propagating `vector-append`
- (experimental) typed database queries

See the [documentation](http://pkg-build.racket-lang.org/doc/trivial/index.html) for the full story.

`trivial` is not currently compatible with untyped Racket.


Install
---

From Github:

```
> git clone https://github.com/bennn/trivial
> raco pkg install ./trivial
```

From the Racket [package server](http://pkgs.racket-lang.org/):

```
> raco pkg install trivial
```

Use `(require trivial)` to import all bindings from this library.
If you get tired of writing colons everywhere, require `trivial/no-colon` instead.


Naming
---

- The library is "trivial" because it solves only the simplest typechecking problems, and does so simply by analyzing a local chunk of source code.

- By convention, we suffix all our exports with a trailing colon.
  This is a hint that the macro will attempt some extra static analysis (including typechecking) at its call-site.


Contributing
---

The perfect feature request would start like:

    "Typed Racket rejects this program, but it is obviously well-typed because ..."

If the right type really is obvious, syntactically, we'll extend this library.


