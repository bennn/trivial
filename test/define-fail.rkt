#lang racket/base
(require trivial/private/test-common
  (only-in typed/racket/base
    ann Zero))

(module+ test (test-compile-error
  #:require trivial/math
  #:exn #rx"mutation not allowed"

  (let-num: ([n 5])
    (set! n 6)
    (ann (-: n 5) Zero))
))
