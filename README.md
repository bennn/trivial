trivial
===
[![Build Status](https://travis-ci.org/bennn/trivial.svg)](https://travis-ci.org/bennn/trivial)
[![Coverage Status](https://coveralls.io/repos/bennn/trivial/badge.svg?branch=master&service=github)](https://coveralls.io/github/bennn/trivial?branch=master)
[![Scribble](https://img.shields.io/badge/Docs-Scribble-blue.svg)](http://docs.racket-lang.org/trivial/index.html)

The `trivial` library implements _type tailoring_ for a variety of standard library operations.

The tailored operations catch more errors statically and raise fewer unnecessary type errors.

```
#lang typed/racket
(require trivial)

(format "binary(~a) = ~b" 3.14 3.14)
;; static type error :)

(let ([match-list (regexp-match #rx"(a*)(b*)" "aaab")])
  (if match-list
    (string-length (second match-list))
    0))
;; not a type error :)

(vector-ref (make-vector 3 #true) 4)
;; static bounds error :)
```

See the documentation for more details.
[http://docs.racket-lang.org/trivial/index.html](http://docs.racket-lang.org/trivial/index.html)


Install
---

From [the package server](https://pkgn.racket-lang.org):
```
  $ raco pkg install trivial
```

From GitHub:
```
  $ git clone https://github.com/bennn/trivial
  $ raco pkg install ./trivial
```


Project Structure
---

- `trivial/main.rkt` defines the API (e.g. the result of `(require trivial)`)
- `trivial/define.rkt`
  `trivial/format.rkt`
  `trivial/function.rkt`
  `trivial/integer.rkt`
  `trivial/list.rkt`
  `trivial/regexp.rkt`
  `trivial/vector.rkt` implement tailored versions of Racket forms
- `trivial/tailoring.rkt` is an API for building new tailorings
- `trivial/private/` main implementation
- `test/` unit tests

