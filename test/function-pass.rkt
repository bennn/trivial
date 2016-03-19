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

  (check-true
    (begin (curry: (lambda ([x : Integer]) x)) #t))

  (check-equal?
    ((curry: (lambda ([x : Integer]) x)) 3)
    3)

  (check-equal?
    (((curry: (lambda ([x : Integer] [y : Integer]) (+ x x y))) 3) 1)
    7)

  (check-equal?
    (((curry: (λ ([x : Any] [y : Any]) x)) 'a) 'b)
    'a)

  (check-equal?
    (map: (lambda ([x : Natural]) (add1 x)) '(8 2 1 3))
    '(9 3 2 4))

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

  (check-equal?
    (map: (lambda ([x : Integer] [y : Integer] [z : Integer])
            (+ (* x y) z))
      '(1 2 3)
      '(4 5 6)
      '(8 9 10))
  '(12 19 28))
)
