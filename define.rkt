#lang typed/racket/base

(provide
  define: let:
)

(require
  (for-syntax
    trivial/private/common
    racket/base)
  (only-in trivial/private/math
    num-define
    num-let)
  (only-in trivial/private/regexp
    rx-define
    rx-let)
  (only-in trivial/private/vector
    vec-define
    vec-let))

(define-syntax define: (make-keyword-alias 'define
  (lambda (stx)
    (or (num-define stx)
        (rx-define stx)
        (vec-define stx)))))
(define-syntax let: (make-keyword-alias 'let
  (lambda (stx)
    (or (num-let stx)
        (rx-let stx)
        (vec-let stx)))))
