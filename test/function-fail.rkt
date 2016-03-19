#lang racket/base
(require trivial/private/test-common)

(module+ test (test-compile-error
  #:require trivial/function trivial/format
  #:exn #rx"Type Checker|lambda:|λ:" ;; TODO, bummer to include lambda:
  ((curry: (lambda (x y) x)) 0 1)
  (((curry: (lambda (x y z) z)) 'x) 'y 'z)
  (((curry: (lambda ([x : Integer] [y : Integer]) (+ x x y))) 'a) 'b)
  ((((curry: (λ ([x : Any] [y : Any]) x)) 'a) 'b) 'c)
  (map: (λ ([x : String] [y : String])
            (string-append x y))
          '("hello"))
  (map: (λ ([x : String] [y : String])
          (string-append x y))
          '("hello")
          '("world")
          '("howareya"))
  (map: (λ ([x : String] [y : String])
          (format: "~d ~d" x y))
          '("hello")
          '("world"))
))
