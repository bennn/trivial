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

  (check-equal?
    (let-num: ([n -4] [m 5])
      (ann (+: m n -1) Zero))
    0)

  (check-equal?
    (let ()
      (define-num: n 6)
      (define-num: m -8)
      (ann (+: n 2 m) Zero))
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

  (check-equal?
    (let-num: ([n 4] [m 5])
      (ann (-: m n 1) Zero))
    0)

  (check-equal?
    (let ()
      (define-num: n 6)
      (define-num: m -8)
      (ann (-: n m 14) Zero))
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

  (check-equal?
    (let-num: ([n 4] [m 5])
      (ann (-: (*: m n) 20) Zero))
    0)

  (check-equal?
    (let ()
      (define-num: n 2)
      (define-num: m -8)
      (ann (-: (*: n -2 m) 32) Zero))
    0)


  ;; -- /:
  (check-equal? (ann (/: 0 1) Zero) 0)
  (check-equal? (ann (/: 0 42) Zero) 0)
  (check-equal? (ann (/: 0 1 2 3 4) Zero) 0)
  (check-equal? (ann (/: 9 9) One) 1)

  (check-equal?
    (ann ((lambda ([f : (-> Integer Integer Exact-Rational)]) (f 1 1)) /:) Real)
    1)

  (check-equal?
    (let-num: ([n 4] [m 12])
      (ann (-: (/: m n) 3) Zero))
    0)

  (check-equal?
    (let ()
      (define-num: n 2)
      (define-num: m -8)
      (ann (+: (/: m n) 4) Zero))
    0)


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
  (check-equal? (ann (let ([n 5]) (*: 3 n (+: -1 2))) Natural) 15)
  (check-equal? (ann (let ([n 4]) (/: n n)) Positive-Exact-Rational) 1)

  ;; -- expt
  (check-equal?
    (ann (expt: 5 3) Index)
    125)
  (check-equal?
    (ann (expt: 99 0) One)
    1)
  (check-equal?
    (ann (expt: (+: 5 -5) 78) Zero)
    0)
  (check-equal?
    (ann (-: (expt: (*: 2 2) (expt: 2 2)) 256) Zero)
    0)
  (check-equal?
    (ann (expt: (* 2 2) (expt: 2 2)) Natural) ;; Not an index
    256)
  (check-equal?
    (let-num: ([n1 5] [n2 4])
      (ann (-: (expt: n1 n2) 625) Zero))
    0)
  (check-equal?
    (let ()
      (define-num: n1 8)
      (define-num: n2 2)
      (ann (-: (expt: n1 n2) 64) Zero))
    0)
  (check-true
    (and (ann (lambda ([n : Natural]) (expt: n 0)) (-> Natural One)) #t))
  (check-true
    (and (ann (lambda ([n : Index]) (expt: n 1)) (-> Index Index)) #t))
)
