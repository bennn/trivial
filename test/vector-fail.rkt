#lang racket/base
(require
  trivial/private/test-common)

(module+ test (test-compile-error
  #:require trivial/vector trivial/math
  #:exn #rx"out-of-bounds|Type Checker"

  (vector-ref: (vector 1) 3)

  (let-vector: ([v (vector 1 2 3)])
    (vector-ref: v 3))

  (let ()
    (define-vector: v (vector 3 4))
    (vector-ref: v 9))

  (let-vector: ([v1  (vector 1)])
    (let-vector: ([v2 (vector v1)])
      (vector-ref: (vector-ref: v2 0) 1)))

  (vector-set!: (vector 0) -1 0)

  (vector-set!: (vector 0) 0 "hello") ;; Strong update

  (vector-ref: (vector-map: (lambda (x) x) (vector #t "ha")) 20)

  (vector-ref: (vector 0) -5)

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

  (vector-take: (vector) 1)
  (vector-take: (vector 1 2) 4)
  (vector-take: (vector 'a) -1)

  (vector-take-right: (vector) 1)
  (vector-take-right: (vector 1 2) 4)
  (vector-take-right: (vector 'a) -1)

  (vector-drop: (vector) 1)
  (vector-drop: (vector 1 2) 4)
  (vector-drop: (vector 'a) -1)

  (vector-drop-right: (vector) 1)
  (vector-drop-right: (vector 1 2) 4)
  (vector-drop-right: (vector 'a) -1)

))
