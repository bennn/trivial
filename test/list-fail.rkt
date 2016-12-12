#lang racket/base
(require trivial/private/test-common)

(module+ test (test-compile-error
  #:require trivial
  #:exn #rx"out of range|Type Checker"

  (let ([v1  (list 1)])
    (let ([v2 (list v1)])
      (list-ref (list-ref v2 0) 1)))

  (car '())
  (cdr '())

  (list-ref (list 1) 3)

  (let ([v (list 1 2 3)])
    (list-ref v 3))

  (let ()
    (define v (list 3 4))
    (list-ref v 9))

  (list-ref (map (lambda (x) x) (list #t "ha")) 20)

  (list-ref (list 0) -5)

  (list-ref
   (map add1 (map add1 (map add1 (list 0 0 0))))
   3)

  (list-ref (map symbol->string (list 'a 'b)) 5)

  (list-ref
    (map add1 (map add1 (map add1 (list 0 0 0))))
    3)

  (let ([v (list 0 0 0)]
                [v2 (list 1 2)])
    (list-ref (append v2 v) 8))

  (list-ref (list 1 2 1) 3)

))

;; TODO these raise errors in the test environment,
;;      but "work as expected"
;  (map (λ ([x : String] [y : String])
;            (string-append x y))
;       '("hello"))
;
;  (map (λ ([x : String] [y : String])
;          (string-append x y))
;          '("hello")
;          '("world")
;          '("howareya"))
;
;  (map (λ ([x : String] [y : String])
;          (format "~d ~d" x y))
;          '("hello")
;          '("world"))
