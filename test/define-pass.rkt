#lang typed/racket/base
(require trivial/define trivial/math trivial/regexp trivial/vector)

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
)
