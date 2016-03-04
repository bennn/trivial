#lang racket/base
(require trivial/private/test-common)

(module+ test (test-compile-error
  #:require trivial/function
  #:exn #rx"Type Checker"
  ((curry: (lambda (x y) x)) 0 1)
  (((curry: (lambda (x y z) z)) 'x) 'y 'z)
))
