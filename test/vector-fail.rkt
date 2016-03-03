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

  (let-vector: ([v1  (vector 1)])
    (let-vector: ([v2 (vector v1)])
      (vector-ref: (vector-ref: v2 0) 1)))

  (vector-set!: (vector 0) 0 "hello") ;; Strong update

  (vector-ref: (vector-map: (lambda (x) x) (vector #t "ha")) 20)

  (vector-ref:
    (vector-map: add1 (vector-map: add1 (vector-map: add1 (vector 0 0 0))))
    3)

  (vector-ref: (vector-map!: (lambda (x) x) (vector #t #t)) 4)
  (vector-ref: (vector-map!: symbol->string (vector 'a 'b)) 0)

  (vector-ref:
    (vector-map!: add1 (vector-map!: add1 (vector-map!: add1 (vector 0 0 0))))
    3)

  (let-vector: ([v (vector 0 0 0)]
                [v2 (vector 1 2)])
    (vector-ref: (vector-append: v2 v) 8))

  (vector-ref: (vector->immutable-vector: (vector 1 2 1)) 3)
)))

;; -----------------------------------------------------------------------------

(module+ test
  (require
    rackunit)

  (define (vector-eval stx)
    (lambda () ;; For `check-exn`
      (compile-syntax stx)))

  (for ([rkt (in-list TEST-CASE*)])
    (check-exn #rx"out-of-bounds|Type Checker"
      (vector-eval rkt)))
)
