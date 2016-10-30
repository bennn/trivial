#lang typed/racket/base

;; Constant-folding math operators.
;; Where possible, they simplify their arguments.

(provide + - * / add1 sub1 expt quotient)
(require trivial/private/integer)
