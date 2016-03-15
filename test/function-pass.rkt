#lang typed/racket/base

(module+ test
  (require
    trivial/format
    trivial/function
    typed/rackunit)

  (check-equal?
    (((curry: (lambda (x y) x)) 'x) 'y)
    'x)

  (check-equal?
    ((((curry: (lambda (x y z) z)) 0) 1) 2)
    2)

  (check-equal?
    (((curry: (lambda ([x : Integer] [y : Integer]) 2)) 0) 1)
    2)

  (check-equal?
    (((curry: (lambda ([x : Integer] [y : Integer]) (+ x x y))) 3) 1)
    7)

  (check-equal?
    (((curry: (λ ([x : Any] [y : Any]) x)) 'a) 'b)
    'a)

  (check-equal?
    (map: (λ ([x : String] [y : String])
            (string-append x y))
          '("hello")
          '("world"))
    '("helloworld"))

  (check-equal?
    (map: (λ ([x : String] [y : String])
            (format: "~a ~a" x y))
          '("hello")
          '("world"))
    '("hello world"))
)
