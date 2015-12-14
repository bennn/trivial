trivial
===

This library provides ``smarter'' versions of Typed Racket standard library functions.
For example:

```
#lang typed/racket/base

(require trivial)

(printf: "hello, ~a")

;; Compiling yields...
;;
;; format:: arity mismatch;
;;  the expected number of arguments does not match the given number
;;    expected: 1
;;      given: 0
```

The `printf:` (with a colon) checks whether its first argument is a string literal.
If so, it parses the string's format sequences for arity and type constraints.
Unless the constraints fail, `printf:` then calls the standard `printf`.

When the first argument to `printf:` is not a string literal, nothing special happens; we just call the standard `printf`.

```
#lang typed/racket/base

(require trivial)

(let ([s "hello, ~a\n"])
  (printf: s)) ;; Non-trivial!

;; Compiles successfully, but throws arity error at runtime
```

Besides `printf:`, this library also provides [macros](http://www.greghendershott.com/fear-of-macros/)
for:

- `regexp-match`, to count the number of groups in a pattern and give the match result a more specific type
- `+`, `-`, `*`, `/`, to reduce constants where possible, yielding results with more specific types.

See the [documentation](http://pkg-build.racket-lang.org/doc/trivial/index.html) for the full story.


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
Each file in the root directory of this repo can be imported specifically, as in:
- `(require trivial/format)`
- `(require trivial/regexp)`
- `(require trivial/math)`

These files only export macros.


Contributing
---

The perfect feature request would start like:

    "Typed Racket rejects this program, but it is obviously well-typed because ..."

If the right type really is obvious, syntactically, we'll extend this library.


