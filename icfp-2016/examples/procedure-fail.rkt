#lang typed/racket/base

;; Apply seems to work fine with arity checking.
;; That's good because I'd rather the type system tracked arity
;;  than me reflecting it from a `define`.

;; -----------------------------------------------------------------------------

;; Works great
;(define (f x y)
;  (+ x y))
;
;(apply f '(1 3 4))

(define (cons3 x y z)
  (cons x (cons y z)))

(map cons3 '() '() '())
