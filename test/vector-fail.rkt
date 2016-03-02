#lang racket/base

(define (expr->typed-module expr)
  #`(module t typed/racket/base
      (require trivial/vector)
      #,expr))

(define TEST-CASE* (map expr->typed-module '(
  (vector-ref: (vector 1) 3)

  (let-vector: ([v (vector 1 2 3)])
    (vector-ref: v 3))

  (let ()
    (define-vector: v (vector 3 4))
    (vector-ref: v 9))

)))

;; -----------------------------------------------------------------------------

(module+ test
  (require
    rackunit)

  (define (vector-eval stx)
    (lambda () ;; For `check-exn`
      (compile-syntax stx)))

  (for ([rkt (in-list TEST-CASE*)])
    (check-exn #rx"vector::|Type Checker"
      (vector-eval rkt)))
)
