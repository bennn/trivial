#lang racket/base
;; TODO why raising wrong exception?

(require trivial/private/test-common
  (only-in typed/racket/base
    ann Zero))

(module+ test (test-compile-error
  #:require trivial/math
  #:exn #rx"mutation not allowed"

  (let-num: ([n 5])
    (set! n 6)
    (ann (-: n 5) Zero))
)
(test-compile-error
  #:require trivial/define trivial/function trivial/format
  #:exn exn:fail? ;;#rx"Type Checker"

  (let: ([f (lambda ([x : String] [y : Integer])
  ;; Error here -- swapped y and x
              (format: "hello(~a) and ~b" y x))])
    (let: ([xs '("hi" "hi" "HI")]
           [ys '(4 3 1)])
    (map: f xs ys)))

))
