#lang typed/racket/base

(module+ test
  (require
    trivial/function
    typed/rackunit)

  (check-equal?
    (((curry: (lambda (x y) x)) 'x) 'y)
    'x)

  (check-equal?
    ((((curry: (lambda (x y z) z)) 0) 1) 2)
    2)
)
