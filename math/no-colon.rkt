#lang typed/racket/base

(provide (all-from-out trivial/math))

(require (rename-in trivial/math
  [+: +]
  [-: -]
  [*: *]
  [/: /]
  [expt: expt]
))
