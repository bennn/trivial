#lang typed/racket/base
(require
  trivial/define
  trivial/format
  trivial/function
  trivial/math
  trivial/regexp
  trivial/vector)

(module+ test
  (require typed/rackunit)

  (check-equal?
    (let ()
      (define: n 3)
      (let: ([m n])
        (ann (-: n m) Zero)))
    0)

  (check-equal?
    (let: ([x (regexp: "(a*)(b*)")])
      (let ([m (regexp-match: x "aaabbb")])
        (if m (string-append (cadr m) (caddr m)) "")))
    "aaabbb")

  (check-equal?
    (let: ([v '#(3 9 2)])
      (ann (-: (vector-length: v) 3) Zero))
    0)

  (check-equal?
    (let: ([f (lambda ([x : String] [y : Integer])
                (format: "hello(~a) and ~b" x y))])
      (let: ([xs '("hi" "hi" "HI")]
             [ys '(4 3 1)])
      (map: f xs ys)))
    '("hello(hi) and 100" "hello(hi) and 11" "hello(HI) and 1"))
)
