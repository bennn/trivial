#lang typed/racket/base

;; Constant-folding math operators.
;; Where possible, they simplify their arguments.

(provide + - * / expt quotient)
(require trivial/private/integer)
