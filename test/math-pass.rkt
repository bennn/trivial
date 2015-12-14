#lang typed/racket/base

;; Well-typed math

(module+ test
  (require
    trivial/math
    typed/rackunit
  )

  ;; -- +:
  (check-equal? (ann (+: 0 0) Zero) 0)
  (check-equal? (ann (+: 1 0) One) 1)
  (check-equal? (ann (+: 0 1) One) 1)
  (check-equal? (ann (+: 3 2) 5) 5)
  (check-equal? (ann (+: 3 1 1) Natural) 5)

  (check-equal?
    (ann ((lambda ([f : (-> Integer Integer Integer)]) (f 0 0)) +:) Integer)
    0)


  ;; -- -:
  (check-equal? (ann (-: 0 0) Zero) 0)
  (check-equal? (ann (-: 1 1) Zero) 0)
  (check-equal? (ann (-: 2 2) Zero) 0)
  (check-equal? (ann (-: 99 97 2) Zero) 0)
  (check-equal? (ann (-: 8 1 3 16) -12) -12)

  (check-equal?
    (ann ((lambda ([f : (-> Integer Integer Integer)]) (f 0 0)) -:) Integer)
    0)


  ;; -- *:
  (check-equal? (ann (*: 0 1315) Zero) 0)
  (check-equal? (ann (*: 11 0) Zero) 0)
  (check-equal? (ann (*: 3 1 3) 9) 9)
  (check-equal? (ann (*: -1 8 4) Negative-Integer) -32)
  (check-equal? (ann (*: 5 1/5 1) One) 1)

  (check-equal?
    (ann ((lambda ([f : (-> Integer Integer Integer)]) (f 0 0)) *:) Integer)
    0)


  ;; -- /:
  (check-equal? (ann (/: 0 1) Zero) 0)
  (check-equal? (ann (/: 0 42) Zero) 0)
  (check-equal? (ann (/: 0 1 2 3 4) Zero) 0)
  (check-equal? (ann (/: 9 9) One) 1)

  ;; We do not catch this statically
  (check-exn exn:fail:contract?
    (lambda () (/: 3 0)))

  (check-equal?
    (ann ((lambda ([f : (-> Integer Integer Exact-Rational)]) (f 1 1)) /:) Real)
    1)


  ;; -- Nested
  (check-equal?
    (ann (+: (+: 1 1) (+: 1 1 1) 1) Index)
    6)
  (check-equal?
    (ann (*: (+: 9 1) (-: 6 3 2 1) 1) Zero)
    0)
  (check-equal?
    (ann (/: (+: 1 2 3 4) (+: (-: 3 2) (+: 1))) Natural)
    5)


  ;; -- Operator works, but we can't fold constants
  (let ([n 0])
    (check-equal? (ann (+: n 1 2 3 4) Natural) 10)
    (check-equal? (ann (-: n n) Integer) 0)
    (check-equal? (ann (*: n 8 1 4 13 1) Natural) 0)
    (check-equal? (ann (/: n 1) Exact-Rational) 0))

  (check-equal? (ann (let ([n 2]) (+: n -2)) Integer) 0)
  (check-equal? (ann (let ([n 5]) (*: n 1/5 1)) Exact-Rational) 1)
  (check-equal? (ann (let ([n 4]) (/: n n)) Positive-Exact-Rational) 1)
  (check-exn #rx"division by zero"
    (lambda () (ann (/: 0 0) Zero))) ;; Same for racket/base

)
